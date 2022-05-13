#define PROT_READ 0x1      /* Page can be read.  */
#define PROT_WRITE 0x2     /* Page can be written.  */
#define PROT_EXEC 0x4      /* Page can be executed.  */
#define MAP_PRIVATE 0x02   /* Changes are private.  */
#define MAP_ANONYMOUS 0x20 /* Don't use a file.  */

typedef unsigned long uint64_t;
typedef long int64_t;

static __attribute__((naked)) uint64_t syscall5(
    uint64_t number,
    uint64_t param1,
    uint64_t param2,
    uint64_t param3,
    uint64_t param4,
    uint64_t param5)
{
  asm(
      "\tmovq %rdi,%rax\n\t"
      "movq %rsi,%rdi\n\t"
      "movq %rdx,%rsi\n\t"
      "movq %rcx,%rdx\n\t"
      "movq %r8,%r10\n\t"
      "movq %r9,%r8\n\t"
      "xorq %r9,%r9\n\t"
      "syscall\n\t"
      "ret");
}

static void exit(uint64_t code)
{
  syscall5(60, code, 0, 0, 0, 0);
}

static void *write(int fd, const void *data, uint64_t nbytes)
{
  return (void *)syscall5(
      1, /* SYS_write */
      fd,
      (uint64_t)data,
      nbytes,
      0, /* ignored */
      0  /* ignored */
  );
}

static void *read(int fd, const void *data, uint64_t nbytes)
{
  return (void *)syscall5(
      0, /* SYS_read */
      fd,
      (uint64_t)data,
      nbytes,
      0, /* ignored */
      0  /* ignored */
  );
}

void *mmap(uint64_t size)
{
  return (void *)syscall5(9, 0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1);
}

void print_int(int64_t value)
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

void print_str(uint64_t p)
{
  write(1, ((uint64_t *)p) + 1, *(uint64_t *)p);
}

#define STR_TRUE "true"
#define STR_FALSE "false"

void print_bool(uint64_t b)
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

uint64_t read_line(char *buf, uint64_t size)
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

int64_t read_int()
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
  int64_t n = 0;
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

void *rootstack_begin;

void initialize(uint64_t rootstack_size, uint64_t heap_size)
{
}

// tag:
// bit0         0=visited, 1=unvisited
// bit1~bit2    0=vector   1=string
//
// vector:
// bit3~bit8    number of fields
// bit9~bit58   pointer mask
//
// string:
// bit3~bit63   string length

void *allocate(uint64_t size, void *rootstack_ptr)
{
  return 0;
}

struct vector_params
{
  uint64_t params[3];
};

void *new_vector(
    uint64_t num_fields,
    uint64_t ptr_mask,
    void *rootstack_ptr,
    struct vector_params params)
{
  uint64_t *p = allocate((num_fields + 1) * sizeof(uint64_t), rootstack_ptr);
  if (p)
  {
    *p = 1 | num_fields << 3 | ptr_mask << 9;
    int n = num_fields > 3 ? 3 : num_fields;
    for (int i = 0; i < n; i++)
    {
      p[i + 1] = params.params[i];
    }
    if (num_fields)
  }
  return p;
}