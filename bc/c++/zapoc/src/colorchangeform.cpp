#include "colorchangeform.hpp"
#include "ui_colorchangeform.h"
#include <QRegExpValidator>

using std::make_unique;

ColorChangeForm::ColorChangeForm(QWidget *parent) :
    QWidget(parent),
    ui(make_unique<Ui::ColorChangeForm>()),
    capitalLetterEditValidator(std::make_unique<QRegExpValidator>(QRegExp("[A-Z]")))
{
    ui->setupUi(this);
    ui->capitalLetterEdit->setValidator(capitalLetterEditValidator.get());
    createActions();

}

ColorChangeForm::~ColorChangeForm() = default;

void ColorChangeForm::removeColorChangeBtnClicked() const {
    this->~ColorChangeForm();
}

void ColorChangeForm::createActions() const {
    connect(ui->removeColorChangeBtn, SIGNAL(clicked()), this, SLOT(removeColorChangeBtnClicked()));
}
