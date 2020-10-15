#ifndef LABELMODEL_H
#define LABELMODEL_H


#include "datasettableproxy.h"

class LabelModel : public DataSetTableProxy
{
	Q_OBJECT
	Q_PROPERTY(int		filteredOut		READ filteredOut									NOTIFY filteredOutChanged		)
	Q_PROPERTY(int		chosenColumn	READ proxyParentColumn	WRITE setProxyParentColumn	NOTIFY proxyParentColumnChanged	)
	Q_PROPERTY(bool		visible			READ visible			WRITE setVisible			NOTIFY visibleChanged			)
	Q_PROPERTY(QString	columnName		READ columnNameQ									NOTIFY columnNameChanged		)

public:
				LabelModel();

	bool		labelNeedsFilter(size_t col);
	std::string columnName(size_t col);
	QString		columnNameQ()			{ return QString::fromStdString(columnName(proxyParentColumn()));	}
	bool		setData(const QModelIndex & index, const QVariant & value, int role)			override;

	void		moveUp(		std::vector<size_t> selection);
	void		moveDown(	std::vector<size_t> selection);
	bool		visible()			const {	return _visible; }
	int			filteredOut()		const;
	int			dataColumnCount()	const;


	Q_INVOKABLE void reverse();
	Q_INVOKABLE void moveUpFromQML(QVariantList selection)		{ moveUp(	convertQVariantList_to_RowVec(selection)); }
	Q_INVOKABLE void moveDownFromQML(QVariantList selection)	{ moveDown(	convertQVariantList_to_RowVec(selection)); }
	Q_INVOKABLE void resetFilterAllows();

	std::vector<bool>			filterAllows(size_t col);
	std::vector<std::string>	labels(size_t col);
	std::vector<size_t>			convertQVariantList_to_RowVec(QVariantList selection);


public slots:
	void filteredOutChangedHandler(int col);
	void setVisible(bool visible);
	void columnAboutToBeRemoved(int column);
	void columnDataTypeChanged(std::string colName);

signals:
	void visibleChanged(bool visible);
	void filteredOutChanged();
	void columnNameChanged();
	void allFiltersReset();
	void labelFilterChanged();

private:
	bool	_visible		= false;
};

#endif // LABELMODEL_H
