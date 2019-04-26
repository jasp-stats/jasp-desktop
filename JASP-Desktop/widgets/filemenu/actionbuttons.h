#ifndef ACTIONBUTTONS_H
#define ACTIONBUTTONS_H

#include <QAbstractListModel>
#include <map>

class ActionButtons : public QAbstractListModel
{
	Q_OBJECT

	Q_PROPERTY(FileOperation selectedAction READ selectedAction WRITE setSelectedAction NOTIFY selectedActionChanged)

public:
	enum FileOperation {Open = 0, Save, SaveAs, ExportResults, ExportData, SyncData, Close, Preferences, About};
	Q_ENUM(FileOperation)

	struct DataRow { FileOperation operation; QString name; bool enabled; };

	enum ActionRoles { NameRole = Qt::UserRole + 1, TypeRole, EnabledRole, SelectedRole };

	explicit ActionButtons(QObject *parent = nullptr);

	//AbstractListModel functions
	int						rowCount(const QModelIndex & = QModelIndex())						const	override { return _data.size(); }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)			const	override;
	bool					setData(const QModelIndex &index, const QVariant &value, int role)			override;
	QHash<int, QByteArray>	roleNames()															const	override;

	FileOperation selectedAction() const { return _selected; }

signals:
	void selectedActionChanged(FileOperation selectedAction);

public slots:
	void setEnabled(ActionButtons::FileOperation operation, bool enabledState);
	void setSelectedAction(FileOperation selectedAction);

private:
	std::vector<DataRow>				_data;
	std::map<FileOperation, size_t>		_opToIndex;
	FileOperation						_selected = Open;
};

#endif // ACTIONBUTTONS_H
