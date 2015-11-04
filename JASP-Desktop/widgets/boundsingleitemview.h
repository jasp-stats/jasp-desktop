#ifndef BOUNDSINGLEITEMVIEW_H
#define BOUNDSINGLEITEMVIEW_H

#include "boundlistview.h"


class boundSingleItemView : public BoundListView
{
   Q_OBJECT

public:
   boundSingleItemView(QWidget *parent = 0);
   virtual int itemCount() const OVERRIDE;

   virtual QSize sizeHint() const OVERRIDE;
   virtual QSize minimumSizeHint() const OVERRIDE;

};

#endif // BOUNDSINGLEITEMVIEW_H
