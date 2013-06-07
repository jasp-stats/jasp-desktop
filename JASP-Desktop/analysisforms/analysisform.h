#ifndef ANALYSISFORM_H
#define ANALYSISFORM_H

#include <QWidget>
#include <QVBoxLayout>
#include <QPushButton>

#include "dataset.h"
#include "options.h"
#include "options/optionfields.h"

#include "availablefields.h"

class AnalysisForm : public QWidget
{
	Q_OBJECT

public:
	explicit AnalysisForm(QWidget *parent = 0);
	void set(Options *options, DataSet *dataSet);

signals:
	void accepted();

protected:

	DataSet *_dataSet;
	Options *_options;

	AvailableFields _availableFields;

	OptionFields *_mainFields;

	virtual void resizeEvent(QResizeEvent *event) override;

protected slots:

	void accept();

private slots:

	void repositionButtonPanel();

private:
	QWidget *_buttonPanel;
	QVBoxLayout *_buttonPanelLayout;
	QPushButton *_okButton;
	QPushButton *_removeButton;
	
	
};

#endif // ANALYSISFORM_H
