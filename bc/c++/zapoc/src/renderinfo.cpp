#include "renderinfo.hpp"

QColor RenderInfo::colorOf(QChar c) const {
    if(_colorChanges.find(c) != _colorChanges.cend()) {
        return _colorChanges.at(c);
    }
    return Qt::black;
}

void RenderInfo::swap(RenderInfo &&other) noexcept {
    std::swap(_lsys, other._lsys);
    std::swap(_colorChanges, other._colorChanges);
    std::swap(_rotationAngle, other._rotationAngle);
    std::swap(_lineLength, other._lineLength);
    std::swap(_directionAngle, other._directionAngle);
    std::swap(_location, other._location);
}
