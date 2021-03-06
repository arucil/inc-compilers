#define PROT_READ 0x1      /* Page can be read.  */
#define PROT_WRITE 0x2     /* Page can be written.  */
#define PROT_EXEC 0x4      /* Page can be executed.  */
#define MAP_PRIVATE 0x02   /* Changes are private.  */
#define MAP_ANONYMOUS 0x20 /* Don't use a file.  */

#define STR(x) #x
#define RT_ERROR_MSG(msg) (__FILE__ ":" STR(__LINE__) ":" STR(msg))
#define RT_FATAL(MSG)                                                      \
  do                                                                       \
  {                                                                        \
    static const char *msg = __FILE__ ":" STR(__LINE__) ":" STR(MSG) "\n"; \
    write(1, msg, strlen(msg));                                            \
    exit(1);                                                               \
  } while (0)
#define RT_FATAL_CODE(MSG, CODE)                                      \
  do                                                                  \
  {                                                                   \
    static const char *msg = __FILE__ ":" STR(__LINE__) ":" STR(MSG); \
    write(1, msg, strlen(msg));                                       \
    rt_print_int(CODE);                                               \
    rt_print_newline();                                               \
    exit(1);                                                          \
  } while (0)

typedef unsigned long uint64_t;
typedef long int64_t;
typedef long ptrdiff_t;

static void exit(uint64_t code) __attribute__((noreturn));

static __attribute__((naked)) uint64_t syscall5(
    uint64_t number,
    uint64_t param1,
    uint64_t param2,
    uint64_t param3,
    uint64_t param4,
    uint64_t param5)
{
  (void)number;
  (void)param1;
  (void)param2;
  (void)param3;
  (void)param4;
  (void)param5;
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
  __builtin_unreachable();
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

static void *mmap(uint64_t size)
{
  return (void *)syscall5(9, 0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1);
}

static int munmap(uint64_t *ptr, uint64_t size)
{
  return (int)syscall5(11, (uint64_t)ptr, size, 0, 0, 0);
}

static int strlen(const char *p)
{
  int len = 0;
  while (*p++)
  {
    len++;
  }
  return len;
}

// tag:
// bit0         0=visited, 1=unvisited
// bit1~bit2    0=vector   1=string   2=array
//
// vector:
// bit3~bit8    number of fields (exluding unit fields)
// bit9~bit59   pointer mask
//
// string:
// bit3~bit63   string length
//
// array:
// bit3         pointer mask
// bit4         unit mask
// bit5~bit63   length

#define align8(x) (((x) + 7) & ~7)

#define VISITED(tag) (((tag)&1) == 0)
#define TYPE(tag) (((tag) >> 1) & 3)
#define TYPE_TUPLE (0)
#define TYPE_STRING (1)
#define TYPE_ARRAY (2)

#define MAKE_TUPLE_TAG(num_fields, ptr_mask) ((ptr_mask) << 9 | (num_fields) << 3 | 0b001)
#define TUPLE_NUM_FIELDS(tag) (((tag) >> 3) & 0b111111)
#define TUPLE_PTR_MASK(tag) ((tag) >> 9)

#define MAKE_STRING_TAG(len) ((len) << 3 | 0b011)
#define STRING_LEN(tag) ((tag) >> 3)

#define MAKE_ARRAY_TAG(ptr_mask, unit_mask, len) ((len) << 5 | (unit_mask) << 4 | (ptr_mask) << 3 | 0b101)
#define ARRAY_LEN(tag) ((tag) >> 5)
#define ARRAY_IS_PTR(tag) (((tag) >> 3) & 1)
#define ARRAY_IS_UNIT(tag) (((tag) >> 4) & 1)

static void print_int(int fd, int64_t value)
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
  write(fd, p, buf + sizeof(buf) - p);
}

void rt_print_int(int64_t value)
{
  print_int(1, value);
}

void rt_print_str_const(uint64_t p)
{
  write(1, ((uint64_t *)p) + 1, *(uint64_t *)p);
}

void rt_print_str(uint64_t p)
{
  write(1, ((uint64_t *)p) + 1, STRING_LEN(*(uint64_t *)p));
}

#define STR_TRUE "true"
#define STR_FALSE "false"

void rt_print_bool(uint64_t b)
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

static void print_newline(int fd)
{
  write(fd, "\n", 1);
}

void rt_print_newline()
{
  write(1, "\n", 1);
}

uint64_t rt_read_line(char *buf, uint64_t size)
{
  char *p = buf;
  for (;;)
  {
    if (!read(0, p, 1) || *p == '\n')
    {
      return p - buf;
    }
    p++;
    if (p - buf == (ptrdiff_t)size)
    {
      return size;
    }
  }
}

int64_t rt_read_int()
{
  static char buf[80];
  char *p = buf;
  char *end = buf + rt_read_line(buf, 80);
  while (p < end && *p == ' ')
  {
    p++;
  }
  if (p == end)
  {
#define INVALID_INT "invalid integer"
    write(2, INVALID_INT, sizeof(INVALID_INT) - 1);
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
    write(2, INVALID_INT, sizeof(INVALID_INT) - 1);
    exit(1);
  }
  return neg ? -n : n;
}

static uint64_t *rootstack_begin;
static void *from_space_begin;
static void *from_space_end;
static void *from_space_ptr;
static void *to_space_begin;

void *rt_initialize(uint64_t rootstack_size, uint64_t heap_size)
{
  rootstack_begin = mmap(rootstack_size);
  if (!rootstack_begin)
  {
    RT_FATAL("could not initialize root stack");
  }
  from_space_begin = mmap(heap_size);
  if (!from_space_begin)
  {
    RT_FATAL("could not initialize heap (from-space)");
  }
  from_space_end = from_space_begin + heap_size;
  from_space_ptr = from_space_begin;
  to_space_begin = mmap(heap_size);
  if (!to_space_begin)
  {
    RT_FATAL("could not initialize heap (to-space)");
  }
  return rootstack_begin;
}

void rt_collect(uint64_t *rootstack_ptr)
{

#define INSPECT_OBJECT(scanp)                 \
  do                                          \
  {                                           \
    uint64_t *old_objp = (uint64_t *)*scanp;  \
    uint64_t old_tag = *old_objp;             \
    if (VISITED(old_tag))                     \
    {                                         \
      *scanp = old_tag; /* forward pointer */ \
    }                                         \
    else                                      \
    {                                         \
      *scanp = (uint64_t)freep;               \
      uint64_t tag = *old_objp;               \
      *old_objp++ = (uint64_t)freep;          \
      *freep++ = tag;                         \
      uint64_t type = TYPE(tag);              \
      uint64_t len;                           \
      switch (type)                           \
      {                                       \
      case TYPE_TUPLE:                        \
        len = TUPLE_NUM_FIELDS(tag);          \
        break;                                \
      case TYPE_STRING:                       \
        len = align8(STRING_LEN(tag)) >> 3;   \
        break;                                \
      case TYPE_ARRAY:                        \
        if (ARRAY_IS_UNIT(tag))               \
          len = 0;                            \
        else                                  \
          len = ARRAY_LEN(tag);               \
        break;                                \
      default:                                \
        RT_FATAL_CODE("invalid type ", type); \
      }                                       \
      for (uint64_t i = 0; i < len; i++)      \
      {                                       \
        *freep++ = *old_objp++;               \
      }                                       \
    }                                         \
  } while (0)

  uint64_t *scanp = to_space_begin;
  uint64_t *freep = to_space_begin;
  for (uint64_t *p = rootstack_begin; p < rootstack_ptr; p++)
  {
    if (!*p)
      continue;
    INSPECT_OBJECT(p);
  }

  while (scanp < freep)
  {
    uint64_t tag = *scanp++;
    uint64_t type = TYPE(tag);
    switch (type)
    {
    case TYPE_TUPLE:
    {
      uint64_t ptr_mask = TUPLE_PTR_MASK(tag);
      for (uint64_t n = TUPLE_NUM_FIELDS(tag); n > 0; n--)
      {
        if (ptr_mask & 1)
        {
          INSPECT_OBJECT(scanp);
        }
        ptr_mask >>= 1;
        scanp++;
      }
      break;
    }
    case TYPE_STRING:
      scanp += align8(STRING_LEN(tag)) >> 3;
      break;
    case TYPE_ARRAY:
    {
      if (ARRAY_IS_UNIT(tag))
      {
        break;
      }
      if (ARRAY_IS_PTR(tag))
      {
        for (uint64_t n = ARRAY_LEN(tag); n > 0; n--)
        {
          INSPECT_OBJECT(scanp);
          scanp++;
        }
      }
      else
      {
        scanp += ARRAY_LEN(tag);
      }
      break;
    }
    default:
      RT_FATAL_CODE("invalid type ", type);
    }
  }

  void *tmp = to_space_begin;
  to_space_begin = from_space_begin;
  from_space_begin = tmp;
  from_space_end = from_space_begin + (from_space_end - to_space_begin);
  from_space_ptr = freep;
}

static void extend_heap(uint64_t size, uint64_t *rootstack_ptr)
{
  uint64_t old_heap_size = from_space_end - from_space_begin;
  uint64_t old_live_size = from_space_ptr - from_space_begin;
  uint64_t new_heap_size = (old_live_size + size) * 7 / 5;
  int ret = munmap(to_space_begin, old_heap_size);
  if (ret)
  {
    RT_FATAL_CODE("munmap() returns ", ret);
  }
  to_space_begin = mmap(new_heap_size);
  if (!to_space_begin)
  {
    RT_FATAL("mmap() failed");
  }
  void *new_from_space_begin = mmap(new_heap_size);
  if (!new_from_space_begin)
  {
    RT_FATAL("mmap() failed");
  }
  uint64_t *to_ptr = new_from_space_begin;
  ptrdiff_t ptr_diff = new_from_space_begin - from_space_begin;
  for (uint64_t *ptr = from_space_begin; (void *)ptr < from_space_ptr;)
  {
    uint64_t tag = *ptr++;
    *to_ptr++ = tag;
    uint64_t type = TYPE(tag);
    switch (type)
    {
    case TYPE_TUPLE:
    {
      uint64_t ptr_mask = TUPLE_PTR_MASK(tag);
      for (uint64_t n = TUPLE_NUM_FIELDS(tag); n > 0; n--)
      {
        if (ptr_mask & 1)
        {
          *to_ptr++ = (uint64_t)((void *)(*ptr++) + ptr_diff);
        }
        else
        {
          *to_ptr++ = *ptr++;
        }
        ptr_mask >>= 1;
      }
      break;
    }
    case TYPE_STRING:
    {
      uint64_t len = align8(STRING_LEN(tag)) >> 3;
      for (uint64_t i = 0; i < len; i++)
      {
        *to_ptr++ = *ptr++;
      }
      break;
    }
    case TYPE_ARRAY:
    {
      if (ARRAY_IS_UNIT(tag))
      {
        break;
      }
      uint64_t len = ARRAY_LEN(tag);
      if (ARRAY_IS_PTR(tag))
      {
        for (uint64_t i = 0; i < len; i++)
        {
          *to_ptr++ = (uint64_t)((void *)(*ptr++) + ptr_diff);
        }
      }
      else
      {
        for (uint64_t i = 0; i < len; i++)
        {
          *to_ptr++ = *ptr++;
        }
      }
      break;
    }
    default:
      RT_FATAL_CODE("invalid type ", type);
    }
  }
  ret = munmap(from_space_begin, old_heap_size);
  if (ret)
  {
    RT_FATAL_CODE("munmap() returns ", ret);
  }
  for (uint64_t *p = rootstack_begin; p < rootstack_ptr; p++)
  {
    if (*p)
    {
      *p += ptr_diff;
    }
  }
  from_space_ptr += ptr_diff;
  from_space_begin = new_from_space_begin;
  from_space_end = from_space_begin + new_heap_size;
}

void *rt_from_space()
{
  return from_space_begin;
}

uint64_t rt_heap_size()
{
  return from_space_end - from_space_begin;
}

static void print_hex(uint64_t n)
{
  static const char hex[] = "0123456789ABCDEF";
  char buf[17];
  for (uint64_t i = 0; i < 16; i++)
  {
    buf[15 - i] = hex[n >> (4 * i) & 15];
  }
  buf[16] = ' ';
  write(1, buf, 17);
}

// void check_invariants(uint64_t index, uint64_t *rootstack_ptr) {
//   for (uint64_t *p = rootstack_begin; p < rootstack_ptr; p++) {
//     if (!*p) continue;
//     if (*p < (uint64_t)from_space_begin || *p >= (uint64_t)from_space_ptr) {
//       rt_print_int(index);
//       write(1, ": rootstack offset: ", 20);
//       rt_print_int((uint64_t)p);
//       write(1, ", from_space_begin: ", 20);
//       rt_print_int((uint64_t)from_space_begin);
//       write(1, ", from_space_ptr: ", 18);
//       rt_print_int((uint64_t)from_space_ptr);
//       write(1, ", to_space_begin: ", 18);
//       rt_print_int((uint64_t)to_space_begin);
//       rt_print_newline();
//       exit(1);
//     }
//   }
//   for (uint64_t *p = from_space_begin; (void *)p < from_space_ptr;)
//   {
//     uint64_t tag = *p++;
//     uint64_t type = TYPE(tag);
//     switch (type)
//     {
//     case TYPE_VECTOR:
//     {
//       uint64_t ptr_mask = TUPLE_PTR_MASK(tag);
//       for (uint64_t n = TUPLE_NUM_FIELDS(tag); n > 0; n--)
//       {
//         if (ptr_mask & 1)
//         {
//           void *pp = (void*)*p;
//     if (pp < from_space_begin || pp >= from_space_ptr) {
//       rt_print_int(index * 1000);
//       write(1, ": tag: ", 7);
//       rt_print_int(tag);
//       write(1, ", ptr: ", 7);
//       print_hex((uint64_t)pp);
//       write(1, ", from_space_begin: ", 20);
//       print_hex((uint64_t)from_space_begin);
//       write(1, ", from_space_ptr: ", 18);
//       print_hex((uint64_t)from_space_ptr);
//       write(1, ", to_space_begin: ", 18);
//       print_hex((uint64_t)to_space_begin);
//       rt_print_newline();
//       uint64_t *q = from_space_begin;
//       for (int j = 0; j < 10; j ++) {
//         print_hex((uint64_t)q);
//         write(1, ": ", 2);
//         for (int i = 0; i < 4; i++) {
//           print_hex(*q);
//           q++;
//           if ((void*)q >= from_space_ptr) {exit(1);}
//         }
//         rt_print_newline();
//       }
//       exit(1);
//     }
//         }
//         p++;
//         ptr_mask >>= 1;
//       }
//       break;
//     }
//     case TYPE_STRING:
//     {
//       uint64_t len = align8(STRING_LEN(tag)) >> 3;
//       for (uint64_t i = 0; i < len; i++)
//       {
//         p++;
//       }
//       break;
//     }
//     default:
//       RT_FATAL_CODE("invalid type ", type);
//     }
//   }
// }

void dump(uint64_t index, uint64_t *rootstack_ptr)
{
  rt_print_int(index);
  rt_print_newline();
  write(1, "rootstack: ", 11);
  for (uint64_t *p = rootstack_begin; p < rootstack_ptr; p++)
  {
    print_hex(*p);
  }
  rt_print_newline();
  write(1, "from space: \n", 13);
  uint64_t *q = from_space_begin;
  for (int j = 0; j < 10; j++)
  {
    print_hex((uint64_t)q);
    write(1, ": ", 2);
    for (int i = 0; i < 4; i++)
    {
      print_hex(*q);
      q++;
      if ((void *)q >= from_space_end)
      {
        break;
      }
    }
    rt_print_newline();
    if ((void *)q >= from_space_end)
    {
      break;
    }
  }
  write(1, "from ptr: ", 10);
  print_hex((uint64_t)from_space_ptr);
  rt_print_newline();
}

// size must be multiple of 8, excluding the tag.
void *rt_allocate(uint64_t tag, uint64_t size, uint64_t *rootstack_ptr)
{
  size += 8;
  if (from_space_end - from_space_ptr < (ptrdiff_t)size)
  {
    rt_collect(rootstack_ptr);
  }
  if (from_space_end - from_space_ptr < (ptrdiff_t)size)
  {
    extend_heap(size, rootstack_ptr);
  }
  uint64_t *ptr = from_space_ptr;
  *ptr = tag;
  from_space_ptr += size;
  return ptr;
}

#define copy_chars(len, destp, srcp)         \
  for (uint64_t i = len >> 3; i > 0; i--)    \
  {                                          \
    *(uint64_t *)destp = *(uint64_t *)srcp;  \
    destp = (char *)((uint64_t *)destp + 1); \
    srcp = (void *)((uint64_t *)srcp + 1);   \
  }                                          \
  switch (len & 7)                           \
  {                                          \
  case 7:                                    \
    destp[6] = ((const char *)srcp)[6];      \
    /* fall through */                       \
  case 6:                                    \
    destp[5] = ((const char *)srcp)[5];      \
    /* fall through */                       \
  case 5:                                    \
    destp[4] = ((const char *)srcp)[4];      \
    /* fall through */                       \
  case 4:                                    \
    destp[3] = ((const char *)srcp)[3];      \
    /* fall through */                       \
  case 3:                                    \
    destp[2] = ((const char *)srcp)[2];      \
    /* fall through */                       \
  case 2:                                    \
    destp[1] = ((const char *)srcp)[1];      \
    /* fall through */                       \
  case 1:                                    \
    destp[0] = ((const char *)srcp)[0];      \
  }

void *rt_new_string(uint64_t len, const char *chars, uint64_t *rootstack_ptr)
{
  uint64_t len_aligned = align8(len);
  uint64_t *s = rt_allocate(MAKE_STRING_TAG(len), len_aligned, rootstack_ptr);
  char *ptr = (char *)(s + 1);
  copy_chars(len, ptr, chars);
  return s;
}

void *rt_alloc_string(uint64_t len, uint64_t *rootstack_ptr)
{
  return rt_allocate(MAKE_STRING_TAG(len), align8(len), rootstack_ptr);
}

void *rt_fill_array(uint64_t *ptr, uint64_t init)
{
  for (uint64_t i = ARRAY_LEN(*ptr); i > 0; i--)
  {
    ptr[i] = init;
  }
  return ptr;
}

void rt_copy_string(void *dest, void *src, uint64_t offset)
{
  uint64_t len1 = STRING_LEN(*(uint64_t *)src);
  const char *p = (const char *)((uint64_t *)src + 1);
  char *q = (char *)((uint64_t *)dest + 1) + offset;
  while (len1 > 0)
  {
    *q++ = *p++;
    len1--;
  }
}

__attribute__((noreturn)) void rt_length_error(int64_t len)
{
#define NEG_LENGTH "negative length: "
  write(2, NEG_LENGTH, sizeof(NEG_LENGTH) - 1);
  print_int(2, len);
  print_newline(2);
  exit(1);
}

__attribute__((noreturn)) void rt_out_of_bounds_error(int64_t index, int64_t len)
{
#define OUT_OF_BOUNDS1 "index out of bounds: "
#define OUT_OF_BOUNDS2 ", length: "
  write(2, OUT_OF_BOUNDS1, sizeof(OUT_OF_BOUNDS1) - 1);
  print_int(2, index);
  write(2, OUT_OF_BOUNDS2, sizeof(OUT_OF_BOUNDS2) - 1);
  print_int(2, len);
  print_newline(2);
  exit(1);
}

__attribute__((noreturn)) void rt_div_by_0_error()
{
#define DIV_BY_0 "division by zero"
  write(2, DIV_BY_0, sizeof(DIV_BY_0) - 1);
  print_newline(2);
  exit(1);
}