#include "readstat_custom_io.h"
#include "boost/nowide/cstdio.hpp"

struct jasp_io_ctx
{
	FILE * file = NULL;
};

jasp_io_ctx * localIoCtx = NULL;

readstat_error_t init_io_handlers(readstat_parser_t * parser)
{
	readstat_error_t		retval = READSTAT_OK;

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
	FILE * file = boost::nowide::fopen(path, "rb");
	static_cast<jasp_io_ctx*>(io_ctx)->file = file;

	return file != NULL;
}


int handle_close(void *io_ctx)
{
	FILE * file = static_cast<jasp_io_ctx*>(io_ctx)->file;

	if (file != NULL)	return fclose(file);
	else				return 0;
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

	FILE * file = static_cast<jasp_io_ctx*>(io_ctx)->file;
#ifdef _WIN32
	return _fseeki64(	file, offset, flag);
#else
	return fseek(		file, offset, flag);
#endif
}

ssize_t handle_read(void *buf, size_t nbyte, void *io_ctx)
{
	FILE * file = static_cast<jasp_io_ctx*>(io_ctx)->file;
	ssize_t out = fread(buf, sizeof(char), nbyte, file); //fread might work ok on 64bit stuff because seek fixed that already?

	return out;
}

readstat_error_t handle_update(long file_size, readstat_progress_handler progress_handler, void *user_ctx, void *io_ctx)
{
	return READSTAT_OK;
}
