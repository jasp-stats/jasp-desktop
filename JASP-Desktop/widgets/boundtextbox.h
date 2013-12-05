#ifndef BOUNDTEXTBOX_H
#define BOUNDTEXTBOX_H

#include <QLineEdit>
#include <QValidator>

#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionnumber.h"

#include "bound.h"

class BoundTextBox : public QLineEdit, public Bound
{
	Q_OBJECT
public:
	explicit BoundTextBox(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;
	
signals:
	
protected:
	void keyPressEvent(QKeyEvent *event) OVERRIDE;
	void focusOutEvent(QFocusEvent *event) OVERRIDE;
	void finalise();

private:
	OptionInteger *_integer;
	OptionIntegerArray *_integerArray;
	OptionNumber *_number;

private slots:
	void textEditedHandler(QString text);

private:
	class QIntArrayValidator : public QValidator
	{
	public:
		QIntArrayValidator();
		QValidator::State validate(QString & input, int&pos) const OVERRIDE;
		virtual void fixup(QString &input) const OVERRIDE;

		static std::vector<int> parse(QString &input);
		static QString stringify(std::vector<int> &input);

	};
	
};

#endif // BOUNDTEXTBOX_H
