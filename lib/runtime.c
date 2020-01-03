#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void* __internal__calloc(int a, int b) {
    return calloc(a, b);
}

char* __internal__concat(char* a, char* b) {
    int an = strlen(a), bn = strlen(b);
    char* buf = malloc(an + bn + 1);
    memcpy(buf, a, an);
    memcpy(buf + an, b, bn);
    buf[an + bn] = '\0';
    return buf;
}
    

void printInt(int x) {
  printf("%d\n", x);
}

void printString(const char* s) {
  printf("%s\n", s);
}

void error() {
  fprintf(stderr, "Runtime error");
  exit(-1);
}

int readInt() {
  int x;
  scanf("%d\n", &x);
  return x;
}

char* readString() {
  char* buf = NULL;
  int i;
  i = getline(&buf, &i, stdin);
  buf[i - 1] = '\0'; // cut off the endline
  return buf;
}

