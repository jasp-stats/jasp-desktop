#ifndef WORKSPACEMODEL_H
#define WORKSPACEMODEL_H

#include <QObject>
#include "undostack.h"

class WorkspaceModel : public QObject
{
	Q_OBJECT
	Q_PROPERTY(QString		name			READ name									NOTIFY nameChanged				)
	Q_PROPERTY(QString		description		READ description	WRITE setDescription	NOTIFY descriptionChanged		)
	Q_PROPERTY(QStringList	emptyValues		READ emptyValues							NOTIFY emptyValuesChanged		)

public:
	explicit WorkspaceModel(QObject *parent = nullptr);
	~WorkspaceModel() override { if(_singleton == this) _singleton = nullptr; } ///< the singleton must not dangle: a second MainWindow constructing its own would throw (same pattern as FileMenu::_singleton)

	static WorkspaceModel* singleton() { return _singleton; }

	QStringList emptyValues()		const;
	QString		name()				const;
	QString		description()		const;

	void		setDescription(const QString& description);



signals:
	void emptyValuesChanged();
	void nameChanged();
	void descriptionChanged();

public slots:
	void removeEmptyValue(const QString& value);
	void addEmptyValue(const QString& value);
	void resetEmptyValues();
	void refresh();

private:
	static WorkspaceModel* _singleton;
};

#endif // WORKSPACEMODEL_H
