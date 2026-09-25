R"for_c++_include(
CREATE INDEX IF NOT EXISTS ColumnOrderIdx			ON Columns	(id, dataSet,	colIdx);
CREATE INDEX IF NOT EXISTS LabelOrderPerColumnIdx	ON Labels	(id, columnId,	ordering);
)for_c++_include"