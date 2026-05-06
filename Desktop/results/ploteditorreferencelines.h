#ifndef PLOTEDITORREFERENCELINES_H
#define PLOTEDITORREFERENCELINES_H

#include <QAbstractTableModel>
#include <qqmlintegration.h>
#include <json/json.h>
#include "utils.h"
#include "controls/componentslistbase.h"

namespace PlotEditor
{

class PlotEditorModel;


class References : public QAbstractTableModel
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY(int count READ count NOTIFY countChanged)

public:
	explicit References(PlotEditorModel * model);

	enum ReferenceType  { Point, LineHorizontal, LineVertical};
	Q_ENUM(ReferenceType)

	struct Reference
	{
		QString				text		= "";
		bool				horizontal	= false,
							point		= true;
		double				x			= 0,
							y;
		QString				color		= "black";
		double				linewidth	= 1.0;
		int					linetype	= 0;	// 0=solid, 1=dashed, 2=dotted, 3=dotdash, 4=longdash, 5=twodash
	};

	int						rowCount(	const QModelIndex &parent = QModelIndex())								const	override;
	int						columnCount(const QModelIndex &parent = QModelIndex())								const	override;
	QVariant				data(		const QModelIndex &index, int role = Qt::DisplayRole)					const	override;
	bool					setData(	const QModelIndex &index, const QVariant &value, int role)						override;
	bool					insertRows(int rows, int count, const QModelIndex &parent = QModelIndex())					override;
	bool					removeRows(int rows, int count, const QModelIndex &parent = QModelIndex())					override;
	int						count()	 { return rowCount(); }


	Json::Value				toJson() const;
	void					fromJson(const Json::Value & json);

	Q_INVOKABLE void		setItem(QQuickItem * item);


signals:
	void					somethingChanged();
	void					addToUndoStack();
	void					countChanged();


protected:
	PlotEditorModel		*	_model;
	std::vector<Reference>	_refs;
	ComponentsListBase	*	_item = nullptr;

};

}

#endif // PLOTEDITORREFERENCELINES_H
