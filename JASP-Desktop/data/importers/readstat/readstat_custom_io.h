#ifndef READSTAT_CUSTOM_IO_H
#define READSTAT_CUSTOM_IO_H

#include "readstat_windows_helper.h"
#include "readstat.h"

#ifdef WIN32
extern "C"
{

readstat_error_t	init_io_handlers(readstat_parser_t *parser);
void				io_cleanup();

int					handle_open(const char *path,																void *io_ctx);
int					handle_close(																				void *io_ctx);
readstat_off_t		handle_seek(readstat_off_t offset, readstat_io_flags_t whence,								void *io_ctx);
ssize_t				handle_read(void *buf, size_t nbyte,														void *io_ctx);
readstat_error_t	handle_update(long file_size, readstat_progress_handler progress_handler, void *user_ctx,	void *io_ctx);

}
#endif

#endif // READSTAT_CUSTOM_IO_H
