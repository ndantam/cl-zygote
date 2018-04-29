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
    int sock = -1;

    const char *home = getenv("HOME");
    const char *basename =  ".cl-zygote.sock";
    size_t n = strlen(home) + strlen(basename) + 2;
    char buf[n];
    snprintf(buf,n,"%s/%s",home,basename);

    sock = zyg_connect(buf);

    const char *opt_msg = NULL;

    /* options */
    {
        int c;
        opterr = 0;
        while( (c = getopt( argc, argv, "e:?")) != -1 ) {
            switch(c) {
            case 'e':
                zyg_send_string(sock,optarg);
                opt_msg = optarg;
                break;
            case '?':
                puts( "clz -e EXPRESSION\n"
                      "Evaluate expression via cl-zygote\n"
                      "\n"
                      "Options:\n"
                      "  -e EXPRESSION,    Lisp expression to evaluate"
                    );
                exit(EXIT_SUCCESS);
            }
        }
    }

    if( NULL == opt_msg ) {
        fprintf(stderr, "No expression specified\n");
        exit(EXIT_FAILURE);
    }

    // mark end of expressions
    zyg_send_string(sock,NULL);

    zyg_send_stdio(sock);
    zyg_wait(sock);

    return 0;
}
