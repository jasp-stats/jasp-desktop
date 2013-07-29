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

	virtual void bindTo(Option *option) override;
	
signals:
	
protected:
	void keyPressEvent(QKeyEvent *event) override;
	void focusOutEvent(QFocusEvent *event) override;
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
		QValidator::State validate(QString & input, int&pos) const override;
		virtual void fixup(QString &input) const override;

		static std::vector<int> parse(QString &input);
		static QString stringify(std::vector<int> &input);

	};
	
};

#endif // BOUNDTEXTBOX_H
