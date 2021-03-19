#include <QKeyEvent>
#include <iostream>
#include <QMessageBox>
#include <QTextStream>
#include <cmath>
#include <algorithm>

#include "mainwindow.hpp"
#include "ui_mainwindow.h"
#include "colorchangeform.hpp"
#include "ruleform.hpp"
#include "selectcolorbutton.hpp"
#include "lsystem.hpp"
#include "renderinfo.hpp"
#include "turtle.hpp"
#include "badgrammar.hpp"

using std::make_unique;
using std::map;
using std::string;
using std::invalid_argument;
using std::exception;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(make_unique<Ui::MainWindow>()),
    axiomEditValidator(make_unique<QRegExpValidator>(Lsystem::everythingRegex)),
    cross(-100, -100) {
    ui->setupUi(this);
    ui->drawingWidget->initCross(&cross);
    ui->axiomEdit->setValidator(axiomEditValidator.get());
    createActions();
}

MainWindow::~MainWindow() = default;

void MainWindow::crossValueChanged() const {
    QString s;
    QTextStream ss(&s);
    ss << "X = ";
    ss << cross.x();
    ss << ", ";
    ss << " Y = ";
    ss << cross.y();
    ui->startingLocationValLbl->setText(std::move(s));
}

void MainWindow::addRuleBtnClicked() const {
    addRule("","");

}

void MainWindow::addColorChangeBtnClicked() const {
    addColorChange("", Qt::black);
}

void MainWindow::keyPressEvent(QKeyEvent *event) {
    if(event->key() == Qt::Key_Return) {
        actionRenderClicked();
    }
}

void MainWindow::actionRenderClicked() {
    if(cross == QPoint(-100, -100)) {
        QMessageBox::warning(this, "Warning", "Starting point must be defined prior to rendering.");
        return;
    }

    try {
        RenderInfo ri(lsystem(), colorChanges(), ui->rotationAngleSpinBox->value(), directionAngle(), ui->lineLengthSpinBox->value(), cross);
        Turtle t(std::move(ri));
        ui->drawingWidget->setInstructions(t.instructions());
    } catch (exception const &e) {
        QMessageBox::critical(this, "Error", e.what());
        return;
    }
}

void MainWindow::actionClearClicked() {
    ui->drawingWidget->clearInstructions();
    restoreGui();

}

void MainWindow::createActions() const {
    connect(ui->addRuleBtn, SIGNAL(clicked()), this, SLOT(addRuleBtnClicked()));
    connect(ui->addColorChangeBtn, SIGNAL(clicked()), this, SLOT(addColorChangeBtnClicked()));
    connect(this, &MainWindow::notifyCrossVisibilityChanged, ui->drawingWidget, &DrawingWidget::crossVisibilityChanged);
    connect(ui->actionBinary_tree, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionBroken_window, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionDragon_curve, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionFern, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionFlower, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionGosper_s_curve, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionIslands_and_lakes, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionLevy_C_curve, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionKoch_s_anti_snowflake, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionKoch_s_snowflake, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionQuadratic_Koch_s_island, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionMap, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionTree, &QAction::triggered, this, &MainWindow::exampleClicked);
    connect(ui->actionRender, &QAction::triggered, this, &MainWindow::drawingMenuItemClicked);
    connect(ui->actionClear, &QAction::triggered, this, &MainWindow::drawingMenuItemClicked);
    connect(ui->actionCross_visibility, &QAction::triggered, this, &MainWindow::drawingMenuItemClicked);
}

void MainWindow::restoreGui() {
    ui->axiomEdit->setText("");
    ui->rotationAngleSpinBox->setValue(90);
    ui->lineLengthSpinBox->setValue(5);
    ui->directionComboBox->setCurrentIndex(0);
    ui->generationSpinBox->setValue(6);
    cross = QPoint(-100,-100);
    ui->drawingWidget->initCross(&cross);
    ui->startingLocationValLbl->setText("Not yet set");
    clearRules();
    clearColorChanges();
}

void MainWindow::clearRules() const {
    QObjectList scrollItems = ui->rulesScrollAreaContents->children();
    for(auto &scrollItem : scrollItems) {
        if(scrollItem->objectName() == "RuleForm") {
            scrollItem->~QObject();
        }
    }
}
void MainWindow::clearColorChanges() const {
    QObjectList scrollItems = ui->colorChangesScrollAreaContents->children();
    for(auto &scrollItem : scrollItems) {
        if(scrollItem->objectName() == "ColorChangeForm") {
            scrollItem->~QObject();
        }
    }
}

map<QChar, QString> MainWindow::rules() const {
    map<QChar, QString> result;
    QObjectList scrollItems = ui->rulesScrollAreaContents->children();
    for(auto &scrollItem : scrollItems) {
        if(scrollItem->objectName() == "RuleForm") {
             auto letterEdit = scrollItem->findChild<QLineEdit *>("letterEdit");
             QString key = letterEdit->text();

             auto transcriptionRuleEdit = scrollItem->findChild<QLineEdit *>("transcriptionRuleEdit");
             QString value = transcriptionRuleEdit->text();

             if(key == "" || value == "") {
                 continue;
             }

             if(result.find(key[0]) != result.cend()) {
                 throw BadGrammar("Multiple transcription rules of " + key.toStdString() + " were given");
             }
             result[key[0]] = std::move(value);
        }
    }
    return result;
}

map<QChar, QColor> MainWindow::colorChanges() const {
    map<QChar, QColor> result;
    QObjectList scrollItems = ui->colorChangesScrollAreaContents->children();
    for(auto &scrollItem : scrollItems) {
        if(scrollItem->objectName() == "ColorChangeForm") {
            auto capitalLetterEdit = scrollItem->findChild<QLineEdit *>("capitalLetterEdit");
            QString key = capitalLetterEdit->text();

            auto scb = scrollItem->findChild<SelectColorButton *>("colorSelectBtn");

            if(key == "") {
                continue;
            }

            if(result.find(key[0]) != result.cend()) {
                throw invalid_argument("Multiple color changes of " + key.toStdString() + " were given");
            }
            result[key[0]] = scb->getColor();
        }
    }
    return result;
}

int MainWindow::directionAngle() const {
    QString dir = ui->directionComboBox->currentText();
    if(dir == "Right") {
        return 0;
    }
    if(dir == "Down") {
        return 90;
    }
    if(dir == "Left") {
        return 180;
    }
    return -90; //Up
}

Lsystem MainWindow::lsystem() const {
    Lsystem lsys(ui->axiomEdit->text());
    lsys.setRules(rules());
    lsys.nthGeneration(ui->generationSpinBox->value());
    return lsys;
}

void MainWindow::addRule(QString &&c, QString &&transcriptionRule) const {
    auto rf = new RuleForm;
    rf->findChild<QLineEdit *>("letterEdit")->setText(std::move(c));
    rf->findChild<QLineEdit *>("transcriptionRuleEdit")->setText(std::move(transcriptionRule));
    ui->rulesScrollAreaContents->layout()->addWidget(rf);

    //should not leak: https://stackoverflow.com/questions/19331396/how-does-qt-delete-objects-and-what-is-the-best-way-to-store-qobjects
}
void MainWindow::addColorChange(QString &&c, QColor &&color) const {
    auto ccf = new ColorChangeForm;
    ccf->findChild<QLineEdit *>("capitalLetterEdit")->setText(std::move(c));
    ccf->findChild<SelectColorButton *>("colorSelectBtn")->setColor(std::move(color));
    ui->colorChangesScrollAreaContents->layout()->addWidget(ccf);

    //should not leak https://stackoverflow.com/questions/19331396/how-does-qt-delete-objects-and-what-is-the-best-way-to-store-qobjects
}

void MainWindow::exampleClicked() {
    restoreGui();

    auto sender = QObject::sender();

    if(sender->objectName() == "actionDragon_curve") {
        ui->axiomEdit->setText("L");
        ui->rotationAngleSpinBox->setValue(90);
        ui->lineLengthSpinBox->setValue(3);
        ui->directionComboBox->setCurrentIndex(1);
        ui->generationSpinBox->setValue(14);
        setCrossValue(687, 290);
        addRule("L", "L+R+");
        addRule("R", "-L-R");
        addColorChange("L", QColor(255,20,147));
    }
    else if(sender->objectName() == "actionFern") {
        ui->axiomEdit->setText("G");
        ui->rotationAngleSpinBox->setValue(25);
        ui->lineLengthSpinBox->setValue(5);
        ui->directionComboBox->setCurrentIndex(3);
        ui->generationSpinBox->setValue(5);
        setCrossValue(530, 419);
        addRule("G", "F+[[G]-G]-F[-FG]+G");
        addRule("F", "FF");
        addColorChange("G", QColor(0,85,0));
        addColorChange("F", QColor(0,85,0));
    }
    else if(sender->objectName() == "actionFlower") {
        ui->axiomEdit->setText("F");
        ui->rotationAngleSpinBox->setValue(25);
        ui->lineLengthSpinBox->setValue(5);
        ui->directionComboBox->setCurrentIndex(3);
        ui->generationSpinBox->setValue(4);
        setCrossValue(540, 431);
        addRule("F", "F[+F]F[-F]F");
        addColorChange("F", QColor(177,167,93));
    }
    else if(sender->objectName() == "actionGosper_s_curve") {
        ui->axiomEdit->setText("A");
        ui->rotationAngleSpinBox->setValue(60);
        ui->lineLengthSpinBox->setValue(8);
        ui->directionComboBox->setCurrentIndex(0);
        ui->generationSpinBox->setValue(4);
        setCrossValue(584, 12);
        addRule("A", "A-B--B+A++AA+B-");
        addRule("B", "+A-BB--B-A++A+B");
        addColorChange("A", QColor(255,0,0));
        addColorChange("B", QColor(0,0,255));
    }
    else if(sender->objectName() == "actionIslands_and_lakes") {
        ui->axiomEdit->setText("F+F+F+F");
        ui->rotationAngleSpinBox->setValue(90);
        ui->lineLengthSpinBox->setValue(5);
        ui->directionComboBox->setCurrentIndex(0);
        ui->generationSpinBox->setValue(2);
        setCrossValue(456, 306);
        addRule("F", "F+f-FF+F+FF+Ff+FF-f+FF-F-FF-Ff-FFF");
        addRule("f", "ffffff");
    }
    else if(sender->objectName() == "actionLevy_C_curve") {
        ui->axiomEdit->setText("F");
        ui->rotationAngleSpinBox->setValue(45);
        ui->lineLengthSpinBox->setValue(5);
        ui->directionComboBox->setCurrentIndex(2);
        ui->generationSpinBox->setValue(12);
        setCrossValue(687, 110);
        addRule("F", "+F--F+");
    }
    else if(sender->objectName() == "actionKoch_s_anti_snowflake") {
        ui->axiomEdit->setText("F++F++F");
        ui->rotationAngleSpinBox->setValue(60);
        ui->lineLengthSpinBox->setValue(2);
        ui->directionComboBox->setCurrentIndex(0);
        ui->generationSpinBox->setValue(5);
        setCrossValue(291, 429);
        addRule("F", "F+F--F+F");
    }
    else if(sender->objectName() == "actionKoch_s_snowflake") {
        ui->axiomEdit->setText("F++F++F");
        ui->rotationAngleSpinBox->setValue(60);
        ui->lineLengthSpinBox->setValue(4);
        ui->directionComboBox->setCurrentIndex(0);
        ui->generationSpinBox->setValue(4);
        setCrossValue(379, 322);
        addRule("F", "F-F++F-F");
        addColorChange("F", QColor(0,170,255));
    }
    else if(sender->objectName() == "actionQuadratic_Koch_s_island") {
        ui->axiomEdit->setText("F-F-F-F");
        ui->rotationAngleSpinBox->setValue(90);
        ui->lineLengthSpinBox->setValue(1);
        ui->directionComboBox->setCurrentIndex(0);
        ui->generationSpinBox->setValue(4);
        setCrossValue(427, 94);
        addRule("F", "F-F+F+FF-F-F+F");
    }
    else if(sender->objectName() == "actionMap") {
        ui->axiomEdit->setText("F-F-F-F");
        ui->rotationAngleSpinBox->setValue(90);
        ui->lineLengthSpinBox->setValue(2);
        ui->directionComboBox->setCurrentIndex(0);
        ui->generationSpinBox->setValue(6);
        setCrossValue(701, 303);
        addRule("F", "F-FF--F-F");
        addColorChange("F", QColor(130,130,130));
    }
    else { //actionTree
        ui->axiomEdit->setText("M");
        ui->rotationAngleSpinBox->setValue(45);
        ui->lineLengthSpinBox->setValue(3);
        ui->directionComboBox->setCurrentIndex(3);
        ui->generationSpinBox->setValue(6);
        setCrossValue(559, 411);
        addRule("M", "S[+M][-M]SM");
        addRule("S", "SS");
        addColorChange("M", QColor(0,85,0));
        addColorChange("S", QColor(170,85,0));

    }

    actionRenderClicked();
}

void MainWindow::drawingMenuItemClicked() {
    auto sender = QObject::sender();
    if(sender->objectName() == "actionRender") {
        actionRenderClicked();
    }
    else if(sender->objectName() == "actionClear") {
        actionClearClicked();
    }
    else if(sender->objectName() == "actionCross_visibility") {
        bool state = !ui->actionCross_visibility->isChecked();
        ui->actionCross_visibility->setChecked(!state);
        emit notifyCrossVisibilityChanged();
    }
}

void MainWindow::setCrossValue(int x, int y) {
    cross.setX(x);
    cross.setY(y);
    crossValueChanged();
}
