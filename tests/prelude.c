#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct String {
  uint32_t length;
  char *data;
};

typedef struct String String;

// String to_string(char *raw) {
//     uint32_t length = strlen(raw);
//     char *data = malloc(length + 1);
//     strcpy(data, raw);
//     String ret = {length, data};
//     return ret;
// }

String to_string(uint32_t self) {
  char buffer[16];
  uint32_t length = sprintf(buffer, "%d", self) - 1;
  String ret = {length, buffer};
  return ret;
}

String string_plus(String *self, char *right) {
  uint32_t length = self->length + strlen(right);
  char *data = malloc(length + 1);
  strcpy(data, self->data);
  strcpy(data + self->length, right);
  String ret = {length, data};
  return ret;
}

void print(char *text) { printf("%s", text); }

void println(char *text) { printf("%s\n", text); }

void printInt(int32_t n) { printf("%d", n); }

void printlnInt(int32_t n) { printf("%d\n", n); }

String getString() {
  char **buffer = NULL;

  uint32_t length = getline(buffer, NULL, stdin);
  if (length > 0 && (*buffer)[length - 1] == '\n') {
    (*buffer)[length - 1] = 0;
    length--;
  }

  String ret = {length, *buffer};
  return ret;
}

int32_t getInt() {
  int32_t n;
  scanf("%d", &n);
  return n;
}
