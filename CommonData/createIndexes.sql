CREATE INDEX IF NOT EXISTS ColumnOrderIdx			ON Columns	(id, dataSet,	colIdx);
CREATE INDEX IF NOT EXISTS LabelOrderPerColumnIdx	ON Labels	(id, columnId,	ordering);
