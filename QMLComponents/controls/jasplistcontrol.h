//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef JASPLISTCONTROL_H
#define JASPLISTCONTROL_H

#include "jaspcontrol.h"
#include "boundcontrols/boundcontrol.h"
#include "common.h"
#include <QObject>
#include <QVector>
#include <QMap>
#include <QSet>
#include <QAbstractItemModel>
#include "models/listmodel.h"

class Options;
class RowControls;
class Terms;
class SourceItem;
class ColumnTypesModel;

class JASPListControl : public JASPControl
{
	Q_OBJECT

	Q_PROPERTY( ListModel*			model							READ model																			NOTIFY modelChanged					)
	Q_PROPERTY( QVariant			source							READ source								WRITE setSource								NOTIFY sourceChanged				)
	Q_PROPERTY( QVariant			rSource							READ rSource							WRITE setRSource							NOTIFY sourceChanged				)
	Q_PROPERTY( QVariant			values							READ values								WRITE setValues								NOTIFY sourceChanged				)
	Q_PROPERTY( int					count							READ count																			NOTIFY countChanged					)
	Q_PROPERTY( int					maxRows							READ maxRows							WRITE setMaxRows							NOTIFY maxRowsChanged				)
	Q_PROPERTY( QString				optionKey						READ optionKey							WRITE setOptionKey																)
	Q_PROPERTY( bool				addEmptyValue					READ addEmptyValue						WRITE setAddEmptyValue						NOTIFY addEmptyValueChanged			)
	Q_PROPERTY( QString				placeholderText					READ placeholderText					WRITE setPlaceHolderText					NOTIFY placeHolderTextChanged		)
	Q_PROPERTY( bool				containsVariables				READ containsVariables																NOTIFY containsVariablesChanged		)
	Q_PROPERTY( bool				containsInteractions			READ containsInteractions															NOTIFY containsInteractionsChanged	)
	Q_PROPERTY( double				maxTermsWidth					READ maxTermsWidth																	NOTIFY maxTermsWidthChanged			)
	Q_PROPERTY( QQmlComponent*		rowComponent					READ rowComponent						WRITE setRowComponent						NOTIFY rowComponentChanged			)
	Q_PROPERTY( bool				addAvailableVariablesToAssigned	READ addAvailableVariablesToAssigned	WRITE setAddAvailableVariablesToAssigned	NOTIFY addAvailableVariablesToAssignedChanged )
	Q_PROPERTY( bool				allowAnalysisOwnComputedColumns	READ allowAnalysisOwnComputedColumns	WRITE setAllowAnalysisOwnComputedColumns	NOTIFY allowAnalysisOwnComputedColumnsChanged )
	Q_PROPERTY( QStringList			columnsTypes					READ columnsTypes																	NOTIFY columnsTypesChanged					)
	Q_PROPERTY( QStringList			columnsNames					READ columnsNames																	NOTIFY columnsNamesChanged					)
	Q_PROPERTY( QAbstractListModel* allowedTypesModel				READ allowedTypesModel																NOTIFY allowedTypesModelChanged		)
	Q_PROPERTY( bool				allowTypeChange					READ allowTypeChange					WRITE setAllowTypeChange					NOTIFY allowTypeChangeChanged		)
	Q_PROPERTY( int					minNumericLevels				READ minNumericLevels					WRITE setMinNumericLevels					NOTIFY minNumericLevelsChanged		)
	Q_PROPERTY( int					maxNumericLevels				READ maxNumericLevels					WRITE setMaxNumericLevels					NOTIFY maxNumericLevelsChanged		)
	Q_PROPERTY( int					minLevels						READ minLevels							WRITE setMinLevels							NOTIFY minLevelsChanged				)
	Q_PROPERTY( int					maxLevels						READ maxLevels							WRITE setMaxLevels							NOTIFY maxLevelsChanged				)
	Q_PROPERTY( QStringList			levels							READ levels																			NOTIFY levelsChanged				)
	Q_PROPERTY( QStringList			allowedColumns					READ allowedColumns						WRITE setAllowedColumns						NOTIFY allowedColumnsChanged		)
	Q_PROPERTY(	QStringList			allowedColumnsIcons				READ allowedColumnsIcons															NOTIFY allowedColumnsIconsChanged	)


public:
	JASPListControl(QQuickItem* parent);
	
	virtual ListModel			*	model()						const	= 0;
	virtual void					setUpModel();
			void					setUp()						override;
			void					cleanUp()					override;
	
	const QVector<SourceItem*>	&	sourceItems()				const			{ return _sourceItems; }
			void					applyToAllSources(std::function<void(SourceItem *sourceItem, const Terms& terms)> applyThis);

			bool					hasSource()					const			{ return _sourceItems.size() > 0; }
			bool					hasNativeSource()			const;

			JASPControl			*	getRowControl(const QString& key, const QString& name)	const;
	virtual	bool					addRowControl(const QString& key, JASPControl* control);
			bool					hasRowComponent()			const;

			const QString		&	optionKey()					const			{ return _optionKey; }
			JASPControl			*	getChildControl(const QString & key, const QString & name) override;

	Q_INVOKABLE QString				getSourceType(QString name);
	Q_INVOKABLE columnType			getVariableType(const QString& name);

			const QVariant		&	source()					const			{ return _source;				}
			const QVariant		&	values()					const			{ return _values;				}
			const QVariant		&	rSource()					const			{ return _rSource;				}
			QQmlComponent		*	rowComponent()				const			{ return _rowComponent;			}
			int						count();
			int						maxRows()					const			{ return _maxRows;				}
			bool					addEmptyValue()				const			{ return _addEmptyValue;		}
			const QString		&	placeholderText()			const			{ return _placeHolderText;		}
			bool					containsVariables()			const			{ return _containsVariables;	}
			bool					containsInteractions()		const			{ return _containsInteractions;	}
			bool					encodeValue()				const override	{ return containsVariables() || containsInteractions();	}
			bool					useSourceLevels()			const			{ return _useSourceLevels;		}
			void					setUseSourceLevels(bool b)					{ _useSourceLevels = b;			}
			double					maxTermsWidth();
	virtual stringvec				usedVariables()				const;
			bool					addAvailableVariablesToAssigned()	const	{ return _addAvailableVariablesToAssigned;	}
			bool					allowAnalysisOwnComputedColumns()	const	{ return _allowAnalysisOwnComputedColumns;	}
			bool					isTypeAllowed(columnType type)		const;
			columnType				defaultType()						const;
			bool					hasMandatoryType()					const;
	const	QStringList			&	columnsTypes()						const	{ return _columnsTypes;						}
	const	QStringList			&	columnsNames()						const	{ return _columnsNames;						}
	QAbstractListModel			*	allowedTypesModel();
	bool							allowTypeChange()					const	{ return _allowTypeChange;		}
	QStringList						levels()							const;
	int								minLevels()							const	{ return _minLevels;			}
	int								maxLevels()							const	{ return _maxLevels;			}
	int								minNumericLevels()					const	{ return _minNumericLevels;		}
	int								maxNumericLevels()					const	{ return _maxNumericLevels;		}
	const QStringList			&	allowedColumns()					const	{ return _allowedColumns;		}
	QStringList						allowedColumnsIcons()				const;
	bool							mayUseFormula()						const	{ return _mayUseFormula;		}
	bool							useTermsInRSyntax()					const	{ return _useTermsInRSyntax;	}
	void							setMayUseFormula(bool use)					{ _mayUseFormula = use;			}
	void							setUseTermsInRSyntax(bool use)				{ _useTermsInRSyntax = use;		}

signals:
			void					modelChanged();
			void					sourceChanged();
			void					countChanged();
			void					maxRowsChanged();
			void					addEmptyValueChanged();
			void					placeHolderTextChanged();
			void					containsVariablesChanged();
			void					containsInteractionsChanged();
			void					maxTermsWidthChanged();
			void					rowComponentChanged();
			void					addAvailableVariablesToAssignedChanged();
			void					allowAnalysisOwnComputedColumnsChanged();
			void					columnsTypesChanged();
			void					columnsNamesChanged();
			void					allowedTypesModelChanged();
			void					allowTypeChangeChanged();
			void					levelsChanged();
			void					minLevelsChanged();
			void					maxLevelsChanged();
			void					minNumericLevelsChanged();
			void					maxNumericLevelsChanged();
			void					allowedColumnsChanged();
			void					allowedColumnsIconsChanged();

public slots:
			void					setContainsVariables();
			void					setContainsInteractions();

protected slots:
	virtual void					termsChangedHandler();
			void					_termsChangedHandler();
			void					sourceChangedHandler();

			void					setOptionKey(const QString& optionKey)	{ _optionKey = optionKey; }
			bool					checkLevelsConstraints();

protected:
	void							_setInitialized(const Json::Value& value = Json::nullValue)	override;
	void							_setAllowedVariables();
	virtual bool					_checkLevelsConstraints();
	bool							_checkLevelsConstraintsForVariable(const QString& variable);

	GENERIC_SET_FUNCTION(Source,							_source,							sourceChanged,							QVariant		)
	GENERIC_SET_FUNCTION(RSource,							_rSource,							sourceChanged,							QVariant		)
	GENERIC_SET_FUNCTION(Values,							_values,							sourceChanged,							QVariant		)
	GENERIC_SET_FUNCTION(AddEmptyValue,						_addEmptyValue,						addEmptyValueChanged,					bool			)
	GENERIC_SET_FUNCTION(PlaceHolderText,					_placeHolderText,					placeHolderTextChanged,					QString			)
	GENERIC_SET_FUNCTION(RowComponent,						_rowComponent,						rowComponentChanged,					QQmlComponent*	)
	GENERIC_SET_FUNCTION(MaxRows,							_maxRows,							maxRows,								int				)
	GENERIC_SET_FUNCTION(AddAvailableVariablesToAssigned,	_addAvailableVariablesToAssigned,	addAvailableVariablesToAssignedChanged,	bool			)
	GENERIC_SET_FUNCTION(AllowAnalysisOwnComputedColumns,	_allowAnalysisOwnComputedColumns,	allowAnalysisOwnComputedColumnsChanged,	bool			)
	GENERIC_SET_FUNCTION(ColumnsTypes,						_columnsTypes,						columnsTypesChanged,					QStringList		)
	GENERIC_SET_FUNCTION(ColumnsNames,						_columnsNames,						columnsNamesChanged,					QStringList		)
	GENERIC_SET_FUNCTION(AllowTypeChange,					_allowTypeChange,					allowTypeChangeChanged,					bool			)
	GENERIC_SET_FUNCTION(MinLevels,							_minLevels,							minLevelsChanged,						int				)
	GENERIC_SET_FUNCTION(MaxLevels,							_maxLevels,							maxLevelsChanged,						int				)
	GENERIC_SET_FUNCTION(MinNumericLevels,					_minNumericLevels,					minNumericLevelsChanged,				int				)
	GENERIC_SET_FUNCTION(MaxNumericLevels,					_maxNumericLevels,					maxNumericLevelsChanged,				int				)
	GENERIC_SET_FUNCTION(AllowedColumns,					_allowedColumns,					allowedColumnsChanged,					QStringList		)


private:
	void					_setupSources();
	Terms					_getCombinedTerms(SourceItem* sourceToCombine);			
			
protected:
	QVector<SourceItem*>	_sourceItems;
	QString					_optionKey							= "value";
	QVariant				_source;
	QVariant				_rSource;
	QVariant				_values;
	bool					_addEmptyValue						= false,
							_containsVariables					= false,
							_containsInteractions				= false,
							_termsAreInteractions				= false,
							_useSourceLevels					= false,
							_addAvailableVariablesToAssigned	= false,
							_allowAnalysisOwnComputedColumns	= true,
							_allowTypeChange					= false,
							_mayUseFormula						= true,
							_useTermsInRSyntax					= true;

	int						_maxRows							= -1,
							_minNumericLevels					= -1,
							_maxNumericLevels					= -1,
							_minLevels							= -1,
							_maxLevels							= -1;

	QString					_placeHolderText					= tr("<no choice>");
	QQmlComponent		*	_rowComponent						= nullptr;
	QStringList				_columnsTypes,
							_columnsNames,
							_allowedColumns;
	ColumnTypesModel	*	_allowedTypesModel					= nullptr;

};

#endif // JASPLISTCONTROL_H
