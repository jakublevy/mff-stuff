#include <QDebug>
#include <QRegExpValidator>

#include "ruleform.hpp"
#include "ui_ruleform.h"
#include "lsystem.hpp"

using std::make_unique;

RuleForm::RuleForm(QWidget *parent) :
    QWidget(parent),
    ui(make_unique<Ui::RuleForm>()),
    letterEditValidator(make_unique<QRegExpValidator>(QRegExp("[a-zA-Z]"))),
    transcriptionRuleEditValidator(make_unique<QRegExpValidator>(QRegExp(Lsystem::everythingRegex)))
{
    ui->setupUi(this);
    ui->letterEdit->setValidator(letterEditValidator.get());
    ui->transcriptionRuleEdit->setValidator(transcriptionRuleEditValidator.get());
    createActions();
}

RuleForm::~RuleForm() = default;

void RuleForm::removeRuleBtnClicked() const {
    this->~RuleForm();
}

void RuleForm::createActions() const {
    connect(ui->removeRuleBtn, SIGNAL(clicked()), this, SLOT(removeRuleBtnClicked()));
}
