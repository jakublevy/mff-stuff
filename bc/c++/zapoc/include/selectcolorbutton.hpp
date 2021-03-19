//Source: https://stackoverflow.com/questions/18257281/qt-color-picker-widget
//I just slightly modified it.

#ifndef SELECTCOLORBUTTON_HPP
#define SELECTCOLORBUTTON_HPP

#include <QPushButton>
#include <QColor>

//This is just a special button that when clicked shows color dialog and then shows chosen color on its background.

class SelectColorButton : public QPushButton
{
    Q_OBJECT
public:
    SelectColorButton(QWidget *parent);

    void setColor(QColor const &color);
    void setColor(QColor &&color);
    QColor const &getColor();

public slots:
    //Updates the button background to match the variable color.
    void updateColor();

    //Opens color dialog and updates the button color.
    void changeColor();

private:
    QColor color;

    //Connects all signals and slots together.
    void createActions() const;
};

#endif //SELECTCOLORBUTTON_HPP
