#include <stdio.h>
#include <stdlib.h>

void* __internal__calloc(int a, int b) {
    return calloc(a, b);
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
  getline(&buf, &i, stdin);
  return buf;
}

