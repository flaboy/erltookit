#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <getopt.h>
#include <pthread.h>
#include <stdarg.h>

#define STARTING 0
#define RUNNING 1
#define STOPPED 2


#define CONST_STR_LEN(s) s, sizeof(s) - 1
#define mylog(...) if(options.log_facility>=0){sprintf(&logbuffer, __VA_ARGS__ ); syslog(LOG_NOTICE,&logbuffer); }

char logbuffer[256];
int pipe_fd[2];

struct global_option {
    char *user;			/* -h option */
    char *group;			/* -h option */
    int kill_signal;
    int test_port;
    char * daemon;
    int log_facility;
} options;

struct proc_status {
    int running;
    pid_t pid;
    int signal;
    int code;
    char *path;
    char **argv;
    int argc;
    int result;
} proc_stat;


int main(int argc, char *argv[])
{
    char *username = NULL, *groupname = NULL, *pid_file = NULL;
    int i_am_root, o;

    if (argc < 2) {		/* no arguments given */
        usage();
        return -1;
    }

    bzero(&proc_stat, sizeof(proc_stat));
    bzero(&options, sizeof(options));
    
    options.log_facility = -1;
    options.kill_signal= SIGTERM;

    i_am_root = (getuid() == 0);
    while (-1 !=
            (o =
             getopt(argc, argv, "p:u:g:k:d:l:"))) {
        switch (o) {
            case 'h': return usage();
            case 'u': if (i_am_root) { options.user = optarg; } break;
            case 'g': if (i_am_root) { options.group = optarg; } break;
            case 'k': options.kill_signal = strtol(optarg,NULL,10); break;
            case 'p': options.test_port = strtol(optarg, NULL, 10); break;
            case 'd': options.daemon = optarg; break;
            case 'l': options.log_facility = log_facility(optarg); break;
            default: 1;
        }
    }

    printf("port %d\n", options.test_port);

    if (argc - optind < 1) {
        return usage();
    }
    proc_stat.path = argv[optind];
    proc_stat.argv = &argv[optind];
    proc_stat.argc = argc - optind - 1;

    return controller();
}



int start_child()
{
    pid_t mypid = getpid();
    setpgid(mypid, mypid);

    pid_t child;
    child = fork();

    switch (child) {
        case 0:
            close(pipe_fd[1]);
            dup2(pipe_fd[0],0);
            if (proc_stat.argc > 0) {
                return execvp(proc_stat.path, proc_stat.argv);
            } else {
                char *b = NULL;
                b = malloc((sizeof("exec ") - 1) + strlen(proc_stat.path) + 1);
                strcpy(b, "exec ");
                strcat(b, proc_stat.path);
                return execl("/bin/sh", "sh", "-c", b, (char *) NULL);
            }

        case -1:
            fprintf(stderr, "launch: fork failed: %s\n", strerror(errno));
            return -1;

        default:
            close(pipe_fd[0]);
            proc_stat.pid = child;
            int a=1;
            mylog("start child pid=%d",child);
            if(wait_for_service()){
                proc_stat.running = RUNNING;
                wait_for_stop();
                clean_child();
            }else{
                clean_child();
            }
    }
}

void clean_child(){
    if (WIFEXITED(proc_stat.result)) {
        proc_stat.code = WEXITSTATUS(proc_stat.result);
    } else if (WIFSIGNALED(proc_stat.result)) {
        proc_stat.signal = WTERMSIG(proc_stat.result);
    } else {
        proc_stat.code = proc_stat.result;
    }
    proc_stat.running = STOPPED;
}

int wait_for_stop(){
    if(is_running()==0) return 1;
    return waitpid(proc_stat.pid, &proc_stat.result, 0);
}

int is_running(){
    printf("1 %s\n",options.daemon);
    if(waitpid(proc_stat.pid, &proc_stat.result, WNOHANG)==0){
        return 1;
    }else if(options.daemon){
        FILE * fd;
        printf("2 %s\n",options.daemon);
        if(0==(fd=fopen(options.daemon,"r"))){
            return 0;
        }else{
            int new_pid=0;
            char buff[50];
            memset(buff, 0, sizeof(buff));
            fread(&buff,20,1,fd);
            new_pid = strtol(buff, NULL, 10);
            fclose(fd);
            if(proc_stat.pid != new_pid){
                printf("new pid=%d, old=%d\n",new_pid, proc_stat.pid);
                proc_stat.pid = new_pid;
                return is_running();
            }else{
                return 0;
            }
        }
    }else{
        return 0;
    }
}

int wait_for_service(){

    if(options.test_port){

        struct sockaddr_in service;
        service.sin_family = PF_INET;
        service.sin_addr.s_addr = inet_addr("127.0.0.1");
        service.sin_port = htons(options.test_port);
        int conn = -1;

	struct timeval tv = { 0, 100 * 1000 };

        while(1){
            int sockfd = socket(PF_INET, SOCK_STREAM, 0);
            if(sockfd < 0) exit(1);
            select(0, NULL, NULL, NULL, &tv);
            conn = connect(sockfd, &service, sizeof(service));
            close(sockfd);
            if(conn==0){
                return 1;
            }else if(!is_running()){
                return 0;
            }
        }
    }

    return 1;
}

int controller()
{
    int rt, last_status;
    fd_set rd;
    char buff[50];
    pthread_t tid;
    pipe(pipe_fd);
    bzero(&buff,sizeof(buff));
    
    if(options.log_facility>=0){
        openlog("launcher" , LOG_NDELAY,options.log_facility);
    }

    last_status = proc_stat.running = STARTING;
    pthread_create(&tid, NULL, (int *) start_child, NULL);
    struct timeval tv = { 0, 100 * 1000 };

    while (proc_stat.running<STOPPED) {
        if(proc_stat.running!=last_status){
            last_status = proc_stat.running;
            printf("!inservice\n");
        }
        FD_ZERO(&rd);
        FD_SET(0, &rd);
        rt = select(FD_SETSIZE, &rd, NULL, NULL, &tv);
        if (rt == -1) {
            perror("select");
            return -1;
        } else if (FD_ISSET(0, &rd)) {
            read(0, buff, sizeof(buff));
            if(buff[0]==0){
                mylog("kill(%d) %d",  options.kill_signal, proc_stat.pid);
                kill(proc_stat.pid, options.kill_signal);
                waitpid(proc_stat.pid, NULL, 0);
                mylog("stoped: %d", proc_stat.pid);
                exit(0);
            }else{
                mylog(&buff);
                write(pipe_fd[1],&buff,sizeof(buff));
                bzero(&buff,sizeof(buff));
            }
        }
    }

    printf("pid: %d\n", proc_stat.pid);
    printf("stopped %d %d\n", proc_stat.code, proc_stat.signal);
    return 0;
}

int usage()
{
    write(1,
            CONST_STR_LEN
            ("Usage: launch [Options] Program [args]\n"
             "\n" "Options:\n"
             " -u <user>      change to user-id\n"
             " -g <group>     change to group-id (default: primary group of user if -u\n"
             " -k <code>      kill signal (default:3)\n"
             "\nRunning test:\n"
             " -p <port>      test to TCP-port\n"
             "\nProcess:\n"
             " -d <pidfile>   daemon mode\n"
            ));
    return 1;
}

int log_facility(char *name)
{
    if (0 == strcmp(name, "daemon"))
        return LOG_DAEMON;
    if (0 == strcmp(name, "user"))
        return LOG_USER;
    if (0 == strcmp(name, "0"))
        return LOG_LOCAL0;
    switch (atoi(name)) {
        case 1:
            return LOG_LOCAL1;
        case 2:
            return LOG_LOCAL2;
        case 3:
            return LOG_LOCAL3;
        case 4:
            return LOG_LOCAL4;
        case 5:
            return LOG_LOCAL5;
        case 6:
            return LOG_LOCAL6;
        case 7:
            return LOG_LOCAL7;
        default:
            return -1;
    }
}
