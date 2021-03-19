#ifndef COLORCHANGEFORM_HPP
#define COLORCHANGEFORM_HPP

#include <QWidget>
#include <QRegExpValidator>
#include <memory>

//This code is backend for colorchangeform.ui

namespace Ui {
class ColorChangeForm;
}

class ColorChangeForm : public QWidget
{
    Q_OBJECT

public:
    explicit ColorChangeForm(QWidget *parent = nullptr);
    ~ColorChangeForm();

public slots:
    void removeColorChangeBtnClicked() const;

private:
    std::unique_ptr<Ui::ColorChangeForm> ui;

    //Regex validator for capitalLetterEdit
    std::unique_ptr<QRegExpValidator> capitalLetterEditValidator;

    void createActions() const;
};

#endif // COLORCHANGEFORM_HPP
