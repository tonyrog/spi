//
// spi_drv.c   SPI linux / erlang driver
//
//

#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <linux/spi/spidev.h>
#include <linux/types.h>
#include <sys/ioctl.h>

#include "erl_driver.h"
// #include "dthread.h"

#define ATOM(NAME) am_ ## NAME
#define INIT_ATOM(NAME) am_ ## NAME = driver_mk_atom(#NAME)

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

#define PORT_CONTROL_BINARY

#define INT_EVENT(e) ((int)((long)(e)))

#define CMD_OPEN       1
#define CMD_CLOSE      2
#define CMD_TRANSFER   3
#define CMD_RD_MODE    4
#define CMD_RD_BPW     5
#define CMD_RD_SPEED   6
#define CMD_DEBUG      7
#define CMD_WR_MODE    8

static inline uint32_t get_uint32(uint8_t* ptr)
{
    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | (ptr[3]<<0);
    return value;
}

static inline int32_t get_int32(uint8_t* ptr)
{
    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | (ptr[3]<<0);
    return (int32_t) value;
}

static inline uint16_t get_uint16(uint8_t* ptr)
{
    uint16_t value = (ptr[0]<<8) | (ptr[1]<<0);
    return value;
}

static inline uint8_t get_uint8(uint8_t* ptr)
{
    uint8_t value = (ptr[0]<<0);
    return value;
}

static inline void put_uint16(uint8_t* ptr, uint16_t v)
{
    ptr[0] = v>>8;
    ptr[1] = v;
}

static inline void put_uint32(uint8_t* ptr, uint32_t v)
{
    ptr[0] = v>>24;
    ptr[1] = v>>16;
    ptr[2] = v>>8;
    ptr[3] = v;
}

typedef struct spi_dev_t
{
    struct spi_dev_t* next;   // when linked
    int fd;
    uint16_t bus;  // bus number
    uint8_t  chip; // chip number
} spi_dev_t;

// FIXME?: speed up by makeing first an matrix [BUS][CHIP]
// when BUS < 8, CHIP < 8?
typedef struct _spi_ctx_t
{
    ErlDrvPort port;
    spi_dev_t* first;
} spi_ctx_t;

static int  spi_drv_init(void);
static void spi_drv_finish(void);
static void spi_drv_stop(ErlDrvData);
static void spi_drv_output(ErlDrvData, char*, ErlDrvSizeT);
static void spi_drv_event(ErlDrvData d, ErlDrvEvent e,
			  ErlDrvEventData ed);
static void spi_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void spi_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData spi_drv_start(ErlDrvPort, char* command);
static ErlDrvSSizeT spi_drv_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**, ErlDrvSizeT);
static void spi_drv_timeout(ErlDrvData);
static void spi_drv_stop_select(ErlDrvEvent, void*);

ErlDrvTermData am_ok;
ErlDrvTermData am_error;
ErlDrvTermData am_undefined;

static ErlDrvEntry spi_drv_entry;

#define DLOG_DEBUG     7
#define DLOG_INFO      6
#define DLOG_NOTICE    5
#define DLOG_WARNING   4
#define DLOG_ERROR     3
#define DLOG_CRITICAL  2
#define DLOG_ALERT     1
#define DLOG_EMERGENCY 0
#define DLOG_NONE     -1

#ifndef DLOG_DEFAULT
#define DLOG_DEFAULT DLOG_NONE
#endif

#define DLOG(level,file,line,args...) do {				\
	if (((level) == DLOG_EMERGENCY) ||				\
	    ((debug_level >= 0) && ((level) <= debug_level))) {		\
	    int save_errno = errno;					\
	    emit_log((level),(file),(line),args);			\
	    errno = save_errno;						\
	}								\
    } while(0)

#define DEBUGF(args...) DLOG(DLOG_DEBUG,__FILE__,__LINE__,args)
#define INFOF(args...)  DLOG(DLOG_INFO,__FILE__,__LINE__,args)
#define NOTICEF(args...)  DLOG(DLOG_NOTICE,__FILE__,__LINE__,args)
#define WARNINGF(args...)  DLOG(DLOG_WARNING,__FILE__,__LINE__,args)
#define ERRORF(args...)  DLOG(DLOG_ERROR,__FILE__,__LINE__,args)
#define CRITICALF(args...)  DLOG(DLOG_CRITICAL,__FILE__,__LINE__,args)
#define ALERTF(args...)  DLOG(DLOG_ALERT,__FILE__,__LINE__,args)
#define EMERGENCYF(args...)  DLOG(DLOG_EMERGENCY,__FILE__,__LINE__,args)

static int debug_level = DLOG_DEFAULT;

static void emit_log(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if ((level == DLOG_EMERGENCY) ||
	((debug_level >= 0) && (level <= debug_level))) {
	int save_errno = errno;
	va_start(ap, line);
	fmt = va_arg(ap, char*);
	fprintf(stderr, "%s:%d: ", file, line); 
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\r\n");
	va_end(ap);
	errno = save_errno;
    }
}

static spi_dev_t** find_dev(spi_ctx_t* ctx, uint16_t bus, uint8_t  chip) 
{
    spi_dev_t** spp = &ctx->first;

    while(*spp) {
	spi_dev_t* sp = *spp;
	if ((sp->bus == bus) && (sp->chip == chip))
	    return spp;
	spp = &sp->next;
    }
    return NULL;
}

// add new spidev first in list 
static void add_dev(spi_ctx_t* ctx, uint16_t bus, uint8_t  chip,
		    spi_dev_t* sp)
{
    sp->next = ctx->first;
    sp->bus = bus;
    sp->chip = chip;
    ctx->first = sp;
}


/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, void* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
#ifdef PORT_CONTROL_BINARY
	ErlDrvBinary* bin = driver_alloc_binary(len+1);
	if (bin == NULL) 
	    return -1;
	ptr = bin->orig_bytes;	
	*rbuf = (char*) bin;
#else
	if ((ptr = driver_alloc(len+1)) == NULL)
	    return -1;
	*rbuf = ptr;
#endif
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}


// setup global object area
// load atoms etc.

static int spi_drv_init(void)
{
    debug_level = DLOG_DEFAULT;
    DEBUGF("spi_driver_init");
    INIT_ATOM(ok);
    INIT_ATOM(error);
    INIT_ATOM(undefined);
    return 0;
}

// clean up global stuff
static void spi_drv_finish(void)
{
}

static ErlDrvData spi_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    spi_ctx_t* ctx;

    if ((ctx = (spi_ctx_t*)
	 driver_alloc(sizeof(spi_ctx_t))) == NULL) {
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    }
    ctx->port = port;
    ctx->first = NULL;
    DEBUGF("spi_drv: start (%s)", command);
#ifdef PORT_CONTROL_BINARY
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
#endif
    return (ErlDrvData) ctx;
}

static void spi_drv_stop(ErlDrvData d)
{
    spi_ctx_t* ctx = (spi_ctx_t*) d;
    spi_dev_t* sp = ctx->first;
    while(sp) {
	spi_dev_t* spn = sp->next;
	driver_select(ctx->port, (ErlDrvEvent)((long)sp->fd), ERL_DRV_USE, 0);
	driver_free(sp);
	sp = spn;
    }
    driver_free(ctx);
}

static ErlDrvSSizeT spi_drv_ctl(ErlDrvData d, 
				 unsigned int cmd, char* buf0, ErlDrvSizeT len,
				 char** rbuf, ErlDrvSizeT rsize)
{
    spi_ctx_t* ctx = (spi_ctx_t*) d;
    uint8_t* buf = (uint8_t*) buf0;

    DEBUGF("spi_drv: ctl: cmd=%u, len=%d", 
	   cmd, len);
    switch(cmd) {
    case CMD_OPEN: {
	spi_dev_t* sp;
	char path[32];
	uint16_t bus;
	uint8_t  chip;
	int n;
	int fd;

	if (len != 3) goto badarg;
	bus = get_uint16(buf);
	chip = get_uint8(buf+2);
	if (find_dev(ctx, bus, chip) != NULL)
	    goto ok; // already open
	n = snprintf(path, sizeof(path), "/dev/spidev%d.%d", bus, chip);
	if (n >= (int)sizeof(path)) goto badarg;
	if ((fd = open(path, O_RDWR, 0)) < 0)
	    goto error;
	if ((sp = driver_alloc(sizeof(spi_dev_t))) == NULL) {
	    close(fd);
	    errno = ENOMEM;
	    goto error;
	}
	add_dev(ctx, bus, chip, sp);
	sp->fd = fd;
	goto ok;
    }
    
    case CMD_CLOSE: {
	spi_dev_t** spp;
	spi_dev_t* sptr;
	uint16_t bus;
	uint8_t  chip;	
	
	if (len != 3) goto badarg;
	bus = get_uint16(buf);
	chip = get_uint8(buf+2);

	if ((spp = find_dev(ctx, bus, chip)) == NULL)
	    goto ok;
	sptr = *spp;
	driver_select(ctx->port, (ErlDrvEvent)((long)sptr->fd), ERL_DRV_USE, 0);
	*spp = sptr->next; // unlink
	driver_free(sptr);
	goto ok;
    }

    case CMD_TRANSFER: {
	spi_dev_t** spp;
	spi_dev_t* sp;
	uint16_t   bus;
	uint8_t    chip;
	uint32_t   n;
	int i;
	struct spi_ioc_transfer transfer_buffer[256];
	char   rxbuf[4096];  // input buffer
	char* rxptr;

	if (len < 7) goto badarg;
	bus  = get_uint16(buf);
	chip = get_uint8(buf+2);
	if ((spp = find_dev(ctx, bus, chip)) == NULL) {
	    errno = ENOENT;
	    goto error;
	}
	sp = *spp;
	n    = get_uint32(buf+3);
	if (n > 256) goto badarg;  // to many transfers
	// follow buf+3 
	// n*
	//  txsize:32, buflen:32, speed:32, delay:16, bpw:8, cs:8
	//  txbuf:txsize/binary
	//
	buf += 7;
	len -= 7;
	rxptr = rxbuf;
	for (i = 0; i < (int)n; i++) {
	    uint32_t txsize;
	    // uint32_t rlen;
	    if (len < 16) goto badarg;
	    txsize = get_uint32(buf);
	    // rlen   = get_uint32(buf+4); // not used right now
	    transfer_buffer[i].tx_buf        = (__u64)((intptr_t)(buf+16));
	    transfer_buffer[i].rx_buf        = (__u64)((intptr_t)rxptr);
	    transfer_buffer[i].len           = txsize;
	    transfer_buffer[i].speed_hz      = get_uint32(buf+8);
	    transfer_buffer[i].delay_usecs   = get_uint32(buf+12);
	    transfer_buffer[i].bits_per_word = get_uint8(buf+14);
	    transfer_buffer[i].cs_change     = get_uint8(buf+15);
	    buf += 16;
	    len -= 16;
	    if (len < txsize) goto badarg;
	    buf   += txsize;
	    len   -= txsize;
	    rxptr += txsize;
	    if ((rxptr - rxbuf) > sizeof(rxbuf))  goto badarg;
	}
	if (ioctl(sp->fd, SPI_IOC_MESSAGE(n), &transfer_buffer) < (int)n)
	    goto error;
	return ctl_reply(3, rxbuf, rxptr-rxbuf, rbuf, rsize);
    }

    case CMD_RD_MODE: {
	spi_dev_t** spp;
	spi_dev_t* sp;
	uint16_t bus;
	uint8_t  chip;
	uint8_t  tmp8;

	if (len != 3) goto badarg;
	bus = get_uint16(buf);
	chip = get_uint8(buf+2);

	if ((spp = find_dev(ctx, bus, chip)) == NULL) {
	    errno = ENOENT;
	    goto error;
	}
	sp = *spp;
	if (ioctl(sp->fd, SPI_IOC_RD_MODE, &tmp8) < 0)
	    goto error;
	return ctl_reply(1, &tmp8, 1, rbuf, rsize);
    }

    case CMD_WR_MODE: {
	spi_dev_t** spp;
	spi_dev_t* sp;
	uint16_t bus;
	uint8_t  chip;
	uint8_t  tmp8;
	uint8_t  mode;

	if (len != 4) goto badarg;
	bus = get_uint16(buf);
	chip = get_uint8(buf+2);
	tmp8 = get_uint8(buf+3);

	if ((spp = find_dev(ctx, bus, chip)) == NULL) {
	    errno = ENOENT;
	    goto error;
	}
	sp = *spp;
	switch(tmp8) {
	case 0: mode = SPI_MODE_0; break;
	case 1: mode = SPI_MODE_1; break;
	case 2: mode = SPI_MODE_2; break;
	case 3: mode = SPI_MODE_3; break;
	default: goto error;
	}
	if (ioctl(sp->fd, SPI_IOC_WR_MODE, &mode) < 0)
	    goto error;
	return ctl_reply(0, NULL, 0, rbuf, rsize);
    }

    case CMD_RD_BPW: {
	spi_dev_t** spp;
	spi_dev_t* sp;
	uint16_t bus;
	uint8_t  chip;
	uint8_t  tmp8;

	if (len != 3) goto badarg;
	bus = get_uint16(buf);
	chip = get_uint8(buf+2);

	if ((spp = find_dev(ctx, bus, chip)) == NULL) {
	    errno = ENOENT;
	    goto error;
	}
	sp = *spp;
	if (ioctl(sp->fd, SPI_IOC_RD_BITS_PER_WORD, &tmp8) < 0)
	    goto error;
	return ctl_reply(1, &tmp8, 1, rbuf, rsize);
    }

    case CMD_RD_SPEED: {
	spi_dev_t** spp;
	spi_dev_t* sp;
	uint16_t   bus;
	uint8_t    chip;
	uint32_t   tmp32;

	if (len != 3) goto badarg;
	bus = get_uint16(buf);
	chip = get_uint8(buf+2);

	if ((spp = find_dev(ctx, bus, chip)) == NULL) {
	    errno = ENOENT;
	    goto error;
	}
	sp = *spp;
	if (ioctl(sp->fd, SPI_IOC_RD_MAX_SPEED_HZ, &tmp32) < 0)
	    goto error;
	return ctl_reply(4, &tmp32, sizeof(tmp32), rbuf, rsize);
    }

    case CMD_DEBUG: {
	if (len != 4)
	    goto badarg;
	debug_level = get_int32(buf);
	goto ok;
    }


    default:
	goto badarg;
    }

ok:
    return ctl_reply(0, NULL, 0, rbuf, rsize);
badarg:
    errno = EINVAL;
error:
    {
        char* err_str = erl_errno_id(errno);
	return ctl_reply(255, err_str, strlen(err_str), rbuf, rsize);
    }
}


static void spi_drv_output(ErlDrvData d, char* buf, ErlDrvSizeT len)
{
    (void) d;
    (void) buf;
    (void) len;
    // spi_ctx_t* ctx = (spi_ctx_t*) d;
    DEBUGF("spi_drv: output");
}

static void spi_drv_outputv(ErlDrvData d, ErlIOVec *ev)
{
    (void) d;
    (void) ev;
    // spi_ctx_t* ctx = (spi_ctx_t*) d;
    DEBUGF("spi_drv: outputv");
}

static void spi_drv_event(ErlDrvData d, ErlDrvEvent e,
			  ErlDrvEventData ed)
{
    (void) d;
    (void) e;
    (void) ed;
    // spi_ctx_t* ctx = (spi_ctx_t*) d;
    DEBUGF("spi_drv: event called");
}

static void spi_drv_ready_input(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
    // spi_ctx_t* ctx = (spi_ctx_t*) d;
    DEBUGF("spi_drv: ready_input called");
}

static void spi_drv_ready_output(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
    // spi_ctx_t* ctx = (spi_ctx_t*) d;
    DEBUGF("dthread_drv: ready_output called");
}

// operation timed out
static void spi_drv_timeout(ErlDrvData d)
{
    (void) d;
    DEBUGF("spi_drv: timeout");
}

static void spi_drv_stop_select(ErlDrvEvent event, void* arg)
{
    (void) arg;
    DEBUGF("spi_drv: stop_select event=%d", INT_EVENT(event));
    close(INT_EVENT(event));
}

DRIVER_INIT(spi_drv)
{
    ErlDrvEntry* ptr = &spi_drv_entry;

    DEBUGF("spi DRIVER_INIT");

    ptr->driver_name = "spi_drv";
    ptr->init  = spi_drv_init;
    ptr->start = spi_drv_start;
    ptr->stop  = spi_drv_stop;
    ptr->output = spi_drv_output;
    ptr->ready_input  = spi_drv_ready_input;
    ptr->ready_output = spi_drv_ready_output;
    ptr->finish = spi_drv_finish;
    ptr->control = spi_drv_ctl;
    ptr->timeout = spi_drv_timeout;
    ptr->outputv = spi_drv_outputv;
    ptr->ready_async = 0;
    ptr->flush = 0;
    ptr->call = 0;
    ptr->event = spi_drv_event;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = spi_drv_stop_select;
    return ptr;
}
