#ifndef FILTERMODEL_H
#define FILTERMODEL_H

#include <set>
#include <QObject>
#include <QString>


class Filter;
class UndoStack;

///
/// Passthrough for the filter gui
class FilterModel : public QObject
{
	Q_OBJECT

	Q_PROPERTY( QVariantList	filterDropDownList			READ filterDropDownList								NOTIFY filterDropDownListChanged	)
	Q_PROPERTY( QVariantList	filterDropDownAnalysisList	READ filterDropDownAnalysisList						NOTIFY filterDropDownListChanged	)
	Q_PROPERTY( QVariantList	computeFilterDropDownList	READ computeFilterDropDownList						NOTIFY filterDropDownListChanged	)
	Q_PROPERTY( Filter		*	filter						READ filter											NOTIFY filterChanged				)
	Q_PROPERTY( bool			filterVisible				READ filterVisible		WRITE setFilterVisible		NOTIFY filterVisibleChanged			)
	Q_PROPERTY( bool			showEasyFilter				READ showEasyFilter		WRITE setShowEasyFilter		NOTIFY showEasyFilterChanged		)
	Q_PROPERTY( QString			currentFilter				READ currentFilter									NOTIFY filterChanged				)
	Q_PROPERTY( QString			currentFilterTitle			READ currentFilterTitle								NOTIFY filterChanged				)
	Q_PROPERTY( int				currentFilterId				READ currentFilterId	WRITE setCurrentFilterId	NOTIFY filterChanged				)
	

public:
	explicit					FilterModel(QObject * parent = nullptr);

				Filter		*	filter()									const;


				QVariantList	filterDropDownList()						const;
				QVariantList	filterDropDownAnalysisList()				const;
				QVariantList	computeFilterDropDownList()					const;
				bool			hasFilter()									const;


	Q_INVOKABLE bool			isJustGeneratedFilter()						const;
				bool			filterVisible()								const;
				void			setFilterVisible(bool newFilterVisible);
				
				bool			showEasyFilter()							const;
				void			setShowEasyFilter(bool newShowEasyFilter);
				void			reset();
				
				QString			currentFilter()								const;
				int				currentFilterId()							const;
				QString			currentFilterTitle()						const;
				void			setCurrentFilterId(int id);
	Q_INVOKABLE void			renameCurrentFilter(const QString & newName);
	Q_INVOKABLE void			deleteCurrentFilter();
	Q_INVOKABLE	void			addFilter(int dataSetId = -1);		

				
signals:
				void			filterDropDownListChanged();
				void			filterChanged();
				void			filterVisibleChanged();
				void			showEasyFilterChanged();
				void			currentFilterChanged();
				
				
public slots:
				void			applyConstructorJson(	QString constructorJson);
				void			applyRFilter(			QString rFilter);
				void			resetRFilter();
				void			computeColumnSucceeded(QString columnName, QString warning, bool dataChanged);
				void			processFilterResult(QString name);
				void			onFilterChanged();
	
private:
	bool						_filterVisible	= false,
								_showEasyFilter	= true;
};

#endif // FILTERMODEL_H
