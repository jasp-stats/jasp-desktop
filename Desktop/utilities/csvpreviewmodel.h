#ifndef CSVPREVIEWMODEL_H
#define CSVPREVIEWMODEL_H

#include <QAbstractTableModel>
#include <QStringList>
#include <QChar>

class CsvPreviewModel : public QAbstractTableModel
{
	Q_OBJECT
	Q_PROPERTY(QString	rawData		READ rawData	WRITE setRawData	NOTIFY rawDataChanged)
	Q_PROPERTY(QChar	delimiter	READ delimiter	WRITE setDelimiter	NOTIFY delimiterChanged)
	Q_PROPERTY(bool		visible		READ visible	WRITE setVisible	NOTIFY visibleChanged)

public:
	explicit CsvPreviewModel(QObject *parent = nullptr);

	int						rowCount(	const QModelIndex &parent = QModelIndex())				const override;
	int						columnCount(const QModelIndex &parent = QModelIndex())				const override;
	QVariant				data(		const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	QHash<int, QByteArray>	roleNames()															const override;

	
	QString					rawData() const { return _rawData; }
	void					setRawData(const QString &data);

	QChar					delimiter() const { return _delimiter; }
	void					setDelimiter(QChar delim);
	void					setDelimiterFromChar(char delim);
	void					preparePreview(const QString &data, char delimiter);

	bool					visible() const;
	void					setVisible(bool newVisible);
	
public slots:
	void					updateLocale();

signals:
	void					rawDataChanged();
	void					delimiterChanged();
	void					visibleChanged();
	void					clearTableForResize();
	
private:
	void					updateInternalStructure();

	QString					_rawData;
	QChar					_delimiter = ','; // Default comma
	QList<QList<QString>>	_grid; // The parsed data
	bool					_visible = false;
};

#endif // CSVPREVIEWMODEL_H
