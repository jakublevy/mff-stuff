#include <QColorDialog>

#include "selectcolorbutton.hpp"

SelectColorButton::SelectColorButton(QWidget *parent) : QPushButton(parent), color(QColor(0, 0, 0)) {
    createActions();
    updateColor();
}

void SelectColorButton::updateColor() {
    setStyleSheet("background-color: " + color.name());
}

void SelectColorButton::changeColor() {
    this->clearFocus();
    QColor newColor = QColorDialog::getColor(color, parentWidget());
    if (newColor != color && newColor != QColor::Invalid) {
        setColor(newColor);
    }
}

void SelectColorButton::setColor(QColor const &color) {
    this->color = color;
    updateColor();
}

void SelectColorButton::setColor(QColor &&color) {
    this->color = std::move(color);
    updateColor();
}

QColor const &SelectColorButton::getColor() {
    return color;
}

void SelectColorButton::createActions() const {
    connect(this, SIGNAL(clicked()), this, SLOT(changeColor()));
}
