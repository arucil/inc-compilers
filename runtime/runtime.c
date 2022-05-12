#include <inttypes.h>

static __attribute__((naked)) void *syscall5(
    uintptr_t number,
    void *param1,
    void *param2,
    void *param3,
    void *param4,
    void *param5)
{
  asm(
      "movq %rdi,%rax\n\t"
      "movq %rsi,%rdi\n\t"
      "movq %rdx,%rsi\n\t"
      "movq %rcx,%rdx\n\t"
      "movq %r8,%r10\n\t"
      "movq %r9,%r8\n\t"
      "syscall\n\t"
      "ret");
}

static void exit(uintptr_t code)
{
  syscall5(60, (void *)code, 0, 0, 0, 0);
}

static void *write(int fd, const void *data, uintptr_t nbytes)
{
  return syscall5(
      1, /* SYS_write */
      (void *)(intptr_t)fd,
      (void *)data,
      (void *)nbytes,
      0, /* ignored */
      0  /* ignored */
  );
}

static void *read(int fd, const void *data, uintptr_t nbytes)
{
  return syscall5(
      0, /* SYS_read */
      (void *)(intptr_t)fd,
      (void *)data,
      (void *)nbytes,
      0, /* ignored */
      0  /* ignored */
  );
}

void print_int(intptr_t value)
{
  static char buf[22];
  char *p = buf + sizeof(buf);
  int neg = 0;
  if (value < 0)
  {
    neg = 1;
    value = -value;
  }
  else if (value == 0)
  {
    *--p = '0';
  }
  while (value)
  {
    *--p = '0' + value % 10;
    value /= 10;
  }
  if (neg)
  {
    *--p = '-';
  }
  write(1, p, buf + sizeof(buf) - p);
}

void print_str(void *p)
{
  write(1, ((void **)p) + 1, *(uintptr_t *)p);
}

#define STR_TRUE "true"
#define STR_FALSE "false"

void print_bool(uintptr_t b)
{
  if (b)
  {
    write(1, STR_TRUE, sizeof(STR_TRUE) - 1);
  }
  else
  {
    write(1, STR_FALSE, sizeof(STR_FALSE) - 1);
  }
}

void print_newline()
{
  write(1, "\n", 1);
}

uintptr_t read_line(char *buf, uintptr_t size)
{
  char *p = buf;
  for (;;)
  {
    if (!read(0, p, 1) || *p == '\n')
    {
      return p - buf;
    }
    p++;
    if (p - buf == size)
    {
      return size;
    }
  }
}

#define ERR_INVALID_INT "invalid integer\n"

intptr_t read_int()
{
  static char buf[80];
  char *p = buf;
  char *end = buf + read_line(buf, 80);
  while (p < end && *p == ' ')
  {
    p++;
  }
  if (p == end)
  {
    write(2, ERR_INVALID_INT, sizeof(ERR_INVALID_INT) - 1);
    exit(1);
  }
  int neg = 0;
  if (*p == '-')
  {
    neg = 1;
    p++;
  }
  intptr_t n = 0;
  if (p < end && *p >= '0' && *p <= '9')
  {
    while (p < end && *p >= '0' && *p <= '9')
    {
      n = n * 10 + (*p - '0');
      p++;
    }
  }
  if (p < end && *p)
  {
    write(2, ERR_INVALID_INT, sizeof(ERR_INVALID_INT) - 1);
    exit(1);
  }
  return neg ? -n : n;
}