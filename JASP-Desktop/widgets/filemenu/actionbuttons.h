#ifndef ACTIONBUTTONS_H
#define ACTIONBUTTONS_H

#include <QAbstractListModel>
#include <map>
#include "resourcebuttons.h"

class ActionButtons : public QAbstractListModel
{
	Q_OBJECT

	Q_PROPERTY(FileOperation selectedAction READ selectedAction WRITE setSelectedAction NOTIFY selectedActionChanged)

public:
	enum FileOperation {None = 0, Open, Save, SaveAs, ExportResults, ExportData, SyncData, Close, Preferences, About, RCmd};
	Q_ENUM(FileOperation)

	struct DataRow { FileOperation operation; QString name; bool enabled; std::set<ResourceButtons::ButtonType> resourceButtons; };

	enum ActionRoles { NameRole = Qt::UserRole + 1, TypeRole, EnabledRole, SelectedRole, ResourceButtonsRole };

	explicit ActionButtons(QObject *parent = nullptr);

	//AbstractListModel functions
	int						rowCount(const QModelIndex & = QModelIndex())						const	override { return _data.size(); }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)			const	override;
	bool					setData(const QModelIndex &index, const QVariant &value, int role)			override;
	QHash<int, QByteArray>	roleNames()															const	override;

	FileOperation selectedAction() const { return _selected; }

	Q_INVOKABLE	void selectButtonUp();
	Q_INVOKABLE	void selectButtonDown();

	std::set<ResourceButtons::ButtonType> resourceButtonsForButton(FileOperation button);
	void refresh();

signals:
				void selectedActionChanged(FileOperation selectedAction);
	Q_INVOKABLE void buttonClicked(FileOperation selectedAction);

public slots:
	void setEnabled(ActionButtons::FileOperation operation, bool enabledState);
	void setSelectedAction(FileOperation selectedAction);


private:
	void loadButtonData(std::vector<DataRow> & data);

	std::vector<DataRow>				_data;
	std::map<FileOperation, size_t>		_opToIndex;
	FileOperation						_selected = None;
};

#endif // ACTIONBUTTONS_H
