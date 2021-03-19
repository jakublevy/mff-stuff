#ifndef DRAWINGWIDGET_HPP
#define DRAWINGWIDGET_HPP

#include <QDialog>
#include <memory>

#include "drawinginstruction.hpp"

//This represents the main drawing area of application.
//All drawing is done here.

class DrawingWidget : public QWidget
{
    Q_OBJECT

public:
    DrawingWidget(QWidget *parent);
    ~DrawingWidget();

    void initCross(QPoint *c) noexcept { cross = c; }
    void setInstructions(std::unique_ptr<std::vector<DrawingInstruction>> i);
    void clearInstructions();

public slots:
    //Notification from MainWindow about visibility of cross.
    void crossVisibilityChanged();

signals:
    //Used to notify MainWindow about cross location changes.
    void notifyCrossLocationChanged() const;

protected:
    //Renders all instructions supplied using setInstructions method.
    void paintEvent(QPaintEvent *eventArgs);

    //Captures mouse and updates cross location if left button is pressed.
    void mouseMoveEvent(QMouseEvent *eventArgs);

private:
    bool crossVisible = true;
    QPoint *cross = nullptr;
    std::unique_ptr<std::vector<DrawingInstruction>> instructions = nullptr;

    //Connects all signals and slots together.
    void createActions() const;

    void drawCross(QPainter &painter) const;
};

#endif // DRAWINGWIDGET_HPP
