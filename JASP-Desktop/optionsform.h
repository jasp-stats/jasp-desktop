#ifndef OPTIONSFORM_H
#define OPTIONSFORM_H

#include <QWidget>
#include <QSettings>

namespace Ui {
class OptionsForm;
}

class OptionsForm : public QWidget
{
	Q_OBJECT

public:
	explicit OptionsForm(QWidget *parent = 0);
	~OptionsForm();

signals:
	void optionsChanged();

private slots:
	void optionChangedHandler(bool);

private:
	Ui::OptionsForm *ui;

	QSettings _settings;
};

#endif // OPTIONSFORM_H
