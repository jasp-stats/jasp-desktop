#ifndef ACTIONBUTTONS_H
#define ACTIONBUTTONS_H

#include <QAbstractListModel>
#include <map>
#include "resourcebuttons.h"

class ActionButtons : public QAbstractListModel
{
	Q_OBJECT

	Q_PROPERTY(FileOperation	selectedAction	READ selectedAction WRITE setSelectedAction NOTIFY selectedActionChanged)
	Q_PROPERTY(int				width			READ width			WRITE setWidth			NOTIFY widthChanged			)

public:
	enum FileOperation {None = 0, New, Open, Save, SaveAs, ExportResults, ExportData, SyncData, Close, Preferences, Contact, Community, About};
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
	
	int width() const;
	void setWidth(int newWidth);
	
signals:
	void selectedActionChanged(	ActionButtons::FileOperation selectedAction);
	void buttonClicked(			ActionButtons::FileOperation selectedAction);
	
	void widthChanged();
	
public slots:
	void setEnabled(			ActionButtons::FileOperation operation, bool enabledState);
	void setSelectedAction(		ActionButtons::FileOperation selectedAction);

private:
	void loadButtonData();

	std::vector<DataRow>				_data;
	std::map<FileOperation, size_t>		_opToIndex;
	FileOperation						_selected = None;
	int									_width = 0;
};

#endif // ACTIONBUTTONS_H
