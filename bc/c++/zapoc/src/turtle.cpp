#include <stack>
#include <QtMath>

#include "turtle.hpp"
#include "badgrammar.hpp"

using std::unique_ptr;
using std::vector;
using std::stack;
using std::make_unique;

unique_ptr<vector<DrawingInstruction>> Turtle::instructions() const {
    auto result = make_unique<vector<DrawingInstruction>>();
    stack<State> states;
    State currState(info.location(), info.directionAngle());
    for(auto &c : info.sentence()) {
        if(Lsystem::movingChar(c)) {
            QPointF newLoc = newLocation(currState.location(), convertToRadians(currState.directionAngle()), info.lineLength());
            if(Lsystem::drawingChar(c)) {
                result->emplace_back(currState.location(), newLoc, info.colorOf(c));
            }
            currState.setLocation(std::move(newLoc));
        }
        else {
            if(c == '+') {
                currState.setDirectionAngle(currState.directionAngle() - info.rotationAngle());
            }
            else if(c == '-') {
                currState.setDirectionAngle(currState.directionAngle() + info.rotationAngle());
            }
            else if(c == '[') {
                states.push(currState);
            }
            else if(c == ']') {
                if(!states.empty()) {
                    currState = states.top();
                    states.pop();
                }
                else {
                    throw BadGrammar("Underlying L-system grammar is invalid");
                }
            }

        }
    }
    return result;
}

QPointF Turtle::newLocation(QPointF const &pt, qreal const &angle, int distance) noexcept {
    qreal x = pt.x() + qCos(angle) * distance;
    qreal y = pt.y() + qSin(angle) * distance;
    return { std::move(x), std::move(y) };
}

qreal Turtle::convertToRadians(int angle) noexcept {
    return static_cast<qreal>(pi) / 180 * angle;
}
