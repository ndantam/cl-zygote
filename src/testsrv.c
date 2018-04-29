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

#include "zygote.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int main (int argc, char **argv) {
    (void) argc;
    (void) argv;


    int sock;
    {
        const char *home = getenv("HOME");
        const char *basename =  ".cl-zygote.sock";
        size_t n = strlen(home) + strlen(basename) + 2;
        char buf[n];
        snprintf(buf,n,"%s/%s",home,basename);
        sock = zyg_listen(buf);
    }


    while(1) {
        int csock = zyg_accept(sock);

        int pid = fork();

        if( 0 == pid ) {
            size_t n = 0;
            char **msgs = NULL;
            char *msg = NULL;
            while( (msg = zyg_recv_string(csock)) ) {
                msgs = (char**) realloc(msgs, (1+n)*(sizeof(char*)));
                msgs[n] = msg;
                printf("got: %s\n", msg);
                n++;
            }

            zyg_recv_stdio(csock);

            for( size_t i = 0; i < n; i ++ ) {
                printf("msg %lu: %s\n", i, msgs[i]);
            }
            exit(EXIT_SUCCESS);
        }

        close(csock);
    }


    return 0;
}
