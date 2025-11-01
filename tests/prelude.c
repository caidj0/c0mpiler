#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct String {
  char *data;
  uint32_t length;
};

struct FatPtr {
  char *data;
  uint32_t length;
};

typedef struct String String;

// String to_string(char *raw) {
//     uint32_t length = strlen(raw);
//     char *data = malloc(length + 1);
//     strcpy(data, raw);
//     String ret = {length, data};
//     return ret;
// }

void to_string(String *string, uint32_t *self) {
  char *buffer = malloc(16);
  uint32_t length = sprintf(buffer, "%d", *self);
  string->length = length;
  string->data = buffer;
}

void string_plus(String *ret, String *self, char *data, uint32_t length) {
  uint32_t new_length = self->length + length;
  char *new_data = malloc(new_length);
  for (int i = 0; i < self->length; i++) {
    new_data[i] = self->data[i];
  }
  for (int i = 0; i < length; i++) {
    new_data[i + self->length] = data[i];
  }
  ret->data = new_data;
  ret->length = new_length;
}

void print(char *text, uint32_t n) { printf("%.*s", n, text); }

void println(char *text, uint32_t n) { printf("%.*s\n", n, text); }

void printInt(int32_t n) { printf("%d", n); }

void printlnInt(int32_t n) { printf("%d\n", n); }

void getString(String *string) {
  char *buffer = NULL;
  size_t n = 0;

  uint32_t length = getline(&buffer, &n, stdin);
  assert(length != -1);
  if (length > 0 && buffer[length - 1] == '\n') {
    buffer[length - 1] = 0;
    length--;
  }

  string->data = buffer;
  string->length = length;
}

int32_t getInt() {
  int32_t n;
  scanf("%d", &n);
  return n;
}
