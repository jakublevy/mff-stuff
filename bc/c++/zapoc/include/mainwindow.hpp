#ifndef MAINWINDOW_HPP
#define MAINWINDOW_HPP

#include <memory>
#include <map>
#include <QSet>
#include <QRegExpValidator>
#include <QMainWindow>

#include "lsystem.hpp"
#include "ruleform.hpp"
#include "colorchangeform.hpp"

//This class is a backend for the main app window.
//A lot of QT related stuff happens here (reacting to events, filling and extracting data to/from GUI).

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

public slots:
    //Called from DrawingWidget when cross location changes.
    void crossValueChanged() const;

    //Adds empty GUI for new rule (RuleForm)
    void addRuleBtnClicked() const;

    //Adds empty GUI for new color change (ColorChangeForm)
    void addColorChangeBtnClicked() const;

signals:
    //Notifies DrawingWidget about changing visibility of cross.
    void notifyCrossVisibilityChanged() const;

protected:
    //Ensures that both enters reacts as keyboard shortcut for render action.
    void keyPressEvent(QKeyEvent *event);

private:
    std::unique_ptr<Ui::MainWindow> ui;

    //Regex validator for axiomEdit
    std::unique_ptr<QRegExpValidator> axiomEditValidator;

    //location of pink cross
    QPoint cross;

    //Renders inputed L-system
    void actionRenderClicked();

    //Clears all drawing instructions given to DrawingWidget and restores GUI to default state.
    void actionClearClicked();

    //Connects all signals and slots together.
    void createActions() const;

    //Restores GUI to default state.
    void restoreGui();

    //Remove all rules inputed in GUI.
    void clearRules() const;

    //Remove all ColorChanges inputed in GUI.
    void clearColorChanges() const;

    //Extracts all rules from GUI
    std::map<QChar, QString> rules() const;

    //Extracts all color changes from GUI
    std::map<QChar, QColor> colorChanges() const;

    //Gets direction angle from directionComboBox
    int directionAngle() const;

    //Creates Lsystem object with respect to current GUI state.
    Lsystem lsystem() const;

    //Adds GUI filled with supplied parameters for new rule (RuleForm)
    void addRule(QString &&c, QString &&transcriptionRule) const;

    //Adds GUI filled with supplied parameters for new color change (ColorChangeForm)
    void addColorChange(QString &&c, QColor &&color) const;

    //Called when one of the items in menu Examples was clicked.
    void exampleClicked();

    //Called when Render, Clear or Cross from second menu Drawing is clicked.
    void drawingMenuItemClicked();

    //Sets new cross location (used for predefined fractal examples)
    void setCrossValue(int x, int y);
};

#endif // MAINWINDOW_HPP
