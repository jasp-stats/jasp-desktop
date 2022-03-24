#include "readstat_custom_io.h"


#ifdef _WIN32

#include <boost/nowide/cstdio.hpp>
#include <boost/nowide/stackstring.hpp>
#include <fcntl.h>
#include <io.h>

extern "C"
{

struct jasp_io_ctx
{
	int fd = -1;
};

jasp_io_ctx * localIoCtx = NULL;

readstat_error_t init_io_handlers(readstat_parser_t * parser)
{
	readstat_error_t retval = READSTAT_OK;

	if ((retval = readstat_set_open_handler(	parser, handle_open))	!= READSTAT_OK)	return retval;
	if ((retval = readstat_set_close_handler(	parser, handle_close))	!= READSTAT_OK)	return retval;
	if ((retval = readstat_set_seek_handler(	parser, handle_seek))	!= READSTAT_OK)	return retval;
	if ((retval = readstat_set_read_handler(	parser, handle_read))	!= READSTAT_OK)	return retval;
	if ((retval = readstat_set_update_handler(	parser, handle_update))	!= READSTAT_OK)	return retval;

	localIoCtx		= new jasp_io_ctx();
	retval			= readstat_set_io_ctx(parser, (void*) localIoCtx);

	return retval;
}

void io_cleanup()
{
	if(localIoCtx)
		delete localIoCtx;
	localIoCtx = NULL;
}

int handle_open(const char *path, void * io_ctx)
{
	boost::nowide::wstackstring wname;

	if(!wname.convert(path)) {
		errno = EINVAL;
		return 0;
	}

    int fd = _wopen(wname.get(), O_RDONLY | O_BINARY);

	static_cast<jasp_io_ctx*>(io_ctx)->fd = fd;

	return fd >= 0 ? READSTAT_OK : READSTAT_ERROR_OPEN;
}

int handle_close(void *io_ctx)
{
	int fd = static_cast<jasp_io_ctx*>(io_ctx)->fd;

	if (fd >= 0)	return _close(fd);
	else			return 0;
}

readstat_off_t handle_seek(readstat_off_t offset, readstat_io_flags_t whence, void *io_ctx)
{
	int flag = 0;
	switch(whence)
	{
	case READSTAT_SEEK_SET:			flag = SEEK_SET;	break;
	case READSTAT_SEEK_CUR:			flag = SEEK_CUR;	break;
	case READSTAT_SEEK_END:			flag = SEEK_END;	break;
	default:											return -1;
	}

	int fd = static_cast<jasp_io_ctx*>(io_ctx)->fd;

	return _lseeki64(fd, offset, flag);
}

ssize_t handle_read(void *buf, size_t nbyte, void *io_ctx)
{
	int fd = static_cast<jasp_io_ctx*>(io_ctx)->fd;
	ssize_t out = _read(fd, buf, nbyte);

	return out;
}

readstat_error_t handle_update(long file_size, readstat_progress_handler progress_handler, void *user_ctx, void *io_ctx)
{
	return READSTAT_OK;
}

}
#endif
