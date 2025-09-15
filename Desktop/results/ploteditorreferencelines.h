#ifndef PLOTEDITORREFERENCELINES_H
#define PLOTEDITORREFERENCELINES_H

#include <QAbstractTableModel>
#include <qqmlintegration.h>
#include <json/json.h>
#include "utils.h"

namespace PlotEditor
{

class PlotEditorModel;


class References : public QAbstractTableModel
{
	Q_OBJECT
	QML_ELEMENT
	Q_PROPERTY(int viewWidth READ viewWidth WRITE setViewWidth NOTIFY viewWidthChanged)
	
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
	};
	
	int						rowCount(	const QModelIndex &parent = QModelIndex())								const	override;
	int						columnCount(const QModelIndex &parent = QModelIndex())								const	override;
	QVariant				data(		const QModelIndex &index, int role = Qt::DisplayRole)					const	override;
	bool					setData(	const QModelIndex &index, const QVariant &value, int role)						override;
	QVariant				headerData ( int section, Qt::Orientation orientation, int role = Qt::DisplayRole )	const	override;
	bool					insertRows(int rows, int count, const QModelIndex &parent = QModelIndex())					override;
	bool					removeRows(int rows, int count, const QModelIndex &parent = QModelIndex())					override;
	QHash<int, QByteArray>	roleNames()																			const	override;
	Qt::ItemFlags			flags(const QModelIndex &index)														const	override;
	
	
	Json::Value				toJson() const;
	void					fromJson(const Json::Value & json);
	
	int						viewWidth() const;
	void					setViewWidth(int newViewWidth);
	
	
public slots:
	void					setColWidth(int index, int width);
	
protected:
	bool					indexDisabled(const QModelIndex &index) const;
	
signals:	
	void					somethingChanged();
	void					addToUndoStack();
	
	
	void viewWidthChanged();
	
protected:
	PlotEditorModel		*	_model;
	std::vector<Reference>	_refs;
	intvec					_widths;
	int						_viewWidth;
	
};

}

#endif // PLOTEDITORREFERENCELINES_H
