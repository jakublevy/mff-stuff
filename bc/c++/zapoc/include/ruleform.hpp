#ifndef RULEFORM_HPP
#define RULEFORM_HPP

#include <QWidget>
#include <QRegExpValidator>
#include <memory>

//This code is backend for ruleform.ui

namespace Ui {
class RuleForm;
}

class RuleForm : public QWidget
{
    Q_OBJECT

public:
    explicit RuleForm(QWidget *parent = nullptr);
    ~RuleForm();

public slots:
    void removeRuleBtnClicked() const;

private:
    std::unique_ptr<Ui::RuleForm> ui;

    //Regex validator for letterEdit
    std::unique_ptr<QRegExpValidator> letterEditValidator;

    //Regex validator for transcriptionRuleEdit
    std::unique_ptr<QRegExpValidator> transcriptionRuleEditValidator;

    void createActions() const;
};

#endif // RULEFORM_HPP
