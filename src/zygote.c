/*
 * Copyright (c) 2018, Colorado School of Mines
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *     * Redistributions of source code must retain the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials
 *       provided with the distribution.
 *
 *     * Neither the name of copyright holder the names of its
 *       contributors may be used to endorse or promote products
 *       derived from this software without specific prior written
 *       permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <poll.h>

#include <sys/un.h>
#include <sys/types.h>
#include <sys/socket.h>


#include "zygote.h"


int zyg_listen( const char *sun_path )
{


    struct sockaddr_un addr;
    {
        int i = unlink(sun_path);
        if(i && ENOENT != i )
            zyg_fail_perr("unlink");
    }

    int sock = socket(AF_UNIX, SOCK_STREAM, 0);
    if( 0 > sock ) {
        zyg_fail_perr("socket");
    }

    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, sun_path, sizeof(addr.sun_path) - 1);

    if (bind(sock, (struct sockaddr *) &addr, sizeof(struct sockaddr_un)) )
        zyg_fail_perr("bind");


    if (listen(sock, 8) )
        zyg_fail_perr("listen");

    return sock;
}

int zyg_accept( int sock )
{

    int csock = accept(sock, NULL, NULL);
    if( 0 > csock ) {
        zyg_fail_perr("accept");
    }

    return csock;
}

int zyg_recv_fd( int sock )
{
    //size_t nread;
    struct msghdr msg;
    struct cmsghdr *cmsg;
    char buf[CMSG_SPACE(sizeof(int))], dup[256];
    memset(buf, '\0', sizeof(buf));
    struct iovec io = { .iov_base = &dup, .iov_len = sizeof(dup) };

    msg.msg_iov = &io;
    msg.msg_iovlen = 1;
    msg.msg_control = buf;
    msg.msg_controllen = sizeof(buf);


    long r = recvmsg(sock, &msg, 0);
    if (r < 0) {
        zyg_fail_perr("recvmsg");
    } else if( r > 0 ) {
        //fprintf(stderr, "r: %d\n", r);
        //printf("size: %d\n", io.iov_len );
        cmsg = CMSG_FIRSTHDR(&msg);
        int *fdptr = (int *)CMSG_DATA(cmsg);
        return *fdptr;
    }

    return -1;
}

char *zyg_process( int csock )
{
    int in_fd = zyg_recv_fd(csock);
    int out_fd = zyg_recv_fd(csock);
    int err_fd = zyg_recv_fd(csock);

    if( in_fd >= 0 )
        dup2(in_fd, STDIN_FILENO);
    if( out_fd >= 0 )
        dup2(out_fd, STDOUT_FILENO);
    if( err_fd >= 0 )
        dup2(err_fd, STDERR_FILENO);

    printf("This message will be displayed on client stdout\n");



    return (char*)"test message\n";
}

int zyg_send_fd( int sock, int fd )
{
    struct msghdr msg = {0};
    struct cmsghdr *cmsg;
    char buf[CMSG_SPACE(sizeof(int))];
    const char *dup = "hello world";
    memset(buf, '\0', sizeof(buf));
    struct iovec io = { .iov_base = &dup,
                        .iov_len = 1 + strlen(dup) };

    printf("sizeof dup: %ld\n",  io.iov_len);

    msg.msg_iov = &io;
    msg.msg_iovlen = 1;
    msg.msg_control = buf;
    msg.msg_controllen = sizeof (buf);

    cmsg = CMSG_FIRSTHDR(&msg);
    cmsg->cmsg_level = SOL_SOCKET;
    cmsg->cmsg_type = SCM_RIGHTS;
    cmsg->cmsg_len = CMSG_LEN(sizeof(int));

    int *fdptr = (int *)CMSG_DATA(cmsg);
    *fdptr = fd;

    if (sendmsg(sock, &msg, 0) == -1)
        zyg_fail_perr("sendmsg");

    return 0;
}

int zyg_connect( const char *sun_path )
{

    /* Create Socket */

    int sock;
    {
        struct sockaddr_un addr;

        sock = socket(AF_UNIX, SOCK_STREAM, 0);
        if (0 > sock )
            zyg_fail_perr("socket");

        memset(&addr, 0, sizeof(struct sockaddr_un));
        addr.sun_family = AF_UNIX;
        strncpy(addr.sun_path, sun_path, sizeof(addr.sun_path) - 1);

        if (connect(sock, (struct sockaddr *) &addr, sizeof(struct sockaddr_un)))
            zyg_fail_perr("connect");
    }


    /* Send Fd */
    zyg_send_fd(sock, STDIN_FILENO);
    zyg_send_fd(sock, STDOUT_FILENO);
    zyg_send_fd(sock, STDERR_FILENO);

    /* Wait for Close */
    struct pollfd pfd;
    pfd.fd = sock;
    pfd.events = POLLHUP;

    while( poll(&pfd, 1, -1) > 0 ) {
        if( pfd.revents & POLLHUP ) break;
    }

    return 0;
}


int zyg_fail_perr ( const char *s )
{
    perror(s);
    exit(EXIT_FAILURE);
}
