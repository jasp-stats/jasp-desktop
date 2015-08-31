#ifndef FILEEVENT_H
#define FILEEVENT_H

#include <QObject>

class FileEvent : public QObject
{
	Q_OBJECT

public:
	FileEvent(QObject *parent = NULL);

	enum FileMode { FileSave, FileOpen, FileExport, FileClose };

	void setOperation(FileMode fileMode);
	void setPath(const QString &path);
	void setReadOnly();

	FileMode operation() const;
	const QString &path() const;
	bool isReadOnly() const;

	void setComplete(bool success = true, const QString &message = "");

	bool isCompleted() const;
	bool successful() const;
	const QString &message() const;

	void chain(FileEvent *event);

signals:
	void completed(FileEvent *event);

private slots:
	void emitComplete();
	void chainedComplete(FileEvent *event);

private:
	FileMode _operation;
	QString _path;
	bool _readOnly;

	bool _isComplete;
	bool _success;
	QString _message;

	FileEvent *_chainedTo;

};

#endif // FILEEVENT_H
