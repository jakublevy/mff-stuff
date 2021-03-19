#include <QPainter>
#include <QMouseEvent>

#include "drawingwidget.hpp"
#include "mainwindow.hpp"

using std::unique_ptr;
using std::vector;

DrawingWidget::DrawingWidget(QWidget *parent) : QWidget(parent) {
    createActions();
}

DrawingWidget::~DrawingWidget() = default;

void DrawingWidget::setInstructions(unique_ptr<vector<DrawingInstruction>> i) {
    instructions = std::move(i);
    update();
}

void DrawingWidget::clearInstructions() {
    instructions.reset();
    update();
}

void DrawingWidget::crossVisibilityChanged() {
    crossVisible = !crossVisible;
    update();
}

void DrawingWidget::paintEvent(QPaintEvent *) {
    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);
    painter.fillRect(0,0, width(), height(), Qt::white);
    if(crossVisible) {
        drawCross(painter);
    }
    if(instructions) {
        for(auto &item : *instructions) {
            painter.setPen(QPen(item.color(), 1));
            painter.drawLine(item.start(), item.end());
        }
    }
}

void DrawingWidget::mouseMoveEvent(QMouseEvent *eventArgs) {
    if(eventArgs->buttons() == Qt::LeftButton) {
        if(eventArgs->x() >= 0 && eventArgs->x() <= width()) {
            if(eventArgs->y() >= 0 && eventArgs->y() <= height()) {
                *cross = eventArgs->pos();
                emit notifyCrossLocationChanged();
                update();
            }
        }
    }
}

void DrawingWidget::createActions() const {
    connect(this, &DrawingWidget::notifyCrossLocationChanged, dynamic_cast<MainWindow *>(parent()->parent()), &MainWindow::crossValueChanged);
}

void DrawingWidget::drawCross(QPainter &painter) const {
     QColor tmp = painter.pen().color();
     painter.setPen(QPen(Qt::magenta, 3));
     painter.drawLine(cross->x() - 10, cross->y(), cross->x() + 10, cross->y());
     painter.drawLine(cross->x(), cross->y() - 10, cross->x(), cross->y() + 10);
     painter.setPen(tmp);
}
