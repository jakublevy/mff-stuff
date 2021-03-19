#ifndef LSM_MATRIX_H
#define LSM_MATRIX_H

#include <cstddef>
#include <vector>
#include <ostream>
#include <numeric>

template <typename T>
class Matrix {

public:
    using value_type = T;
    using reference = T&;
    using constant_reference = T const &;
    using rvalue_reference = T&&;


    template <typename U>
    friend Matrix<U> operator *(const Matrix<U>& lhs, const Matrix<U>& rhs);

    Matrix(std::size_t m, std::size_t n) : denseMatrix(), w(n), h(m) { denseMatrix.resize(m*n); }
    Matrix(std::initializer_list<std::initializer_list<T>>&& matrixData);

    T& operator [](std::pair<std::size_t, std::size_t> idx);
    const T& operator [](std::pair<std::size_t, std::size_t> idx) const;

    Matrix<T>& operator +=(const Matrix<T>& m);
    Matrix<T>& operator -=(const Matrix<T>& m);
    Matrix<T> operator *=(const Matrix<T>& m);
    Matrix<T>& operator *=(const T& scalar);
    Matrix<T>& operator /=(const T& scalar);


    static Matrix<T> eye(std::size_t m, std::size_t n);
    static Matrix<T> allK(std::size_t m, std::size_t n, std::size_t k);

    std::size_t width() const { return w; }
    std::size_t height() const { return h; }

    void transpose();
    void rowReduce();
    Matrix<T> inverse() const;

    Matrix<T> row(std::size_t m) const;
    Matrix<T> column(std::size_t n) const;

    void setColumnTo(std::size_t n, const Matrix<T>& m);
    void setRowTo(std::size_t m, const Matrix<T>& m1);

    Matrix<T> submatrix(std::size_t i, std::size_t j, std::size_t m, std::size_t n);
    void setSubmatrix(std::size_t i, std::size_t j, const Matrix<T>& m);

    void swapColumns(std::size_t i, std::size_t j);
    void swapRows(std::size_t i, std::size_t j);

    Matrix<T> lsm(const Matrix<T>& b);

    bool isEye() const;
    bool isDiagonal() const;
    bool isUpperTriangular() const;
    bool isLowerTriangular() const;
    bool isSquare() const;
    T det() const; //TODO: implement

private:
    bool hasSameDimensionAs(const Matrix<T>& m) const;
    bool canBeMultipliedWith(const Matrix<T>& m) const;
    void changeDimensions(std::size_t m, std::size_t n);
    static void throwBadMatrixDimensions();
    static void throwNotInvertibleMatrix();

    std::vector<T> ref();

    std::vector<T> denseMatrix;
    std::size_t w;
    std::size_t h;
    bool transposed = false;
};

template<typename T>
Matrix<T>::Matrix(std::initializer_list<std::initializer_list<T>>&& matrixData) {
    size_t w = 0, h = 0;
    for(auto itColumn = matrixData.begin(); itColumn != matrixData.end(); ++itColumn) {
        ++h;
        for(auto itRow = itColumn->begin(); itRow != itColumn->end(); ++itRow) {
            denseMatrix.emplace_back(std::move(*itRow));
        }
        if(w == 0) w = denseMatrix.size();
    }
    if(denseMatrix.size() != w * h) {
        throw std::invalid_argument("Bad initialization matrix.");
    }
    this->w = w;
    this->h = h;
}

template<typename T>
T& Matrix<T>::operator [](std::pair<std::size_t, std::size_t> idx) {
    if(idx.first < 1 || idx.second < 1 || idx.first > h || idx.second > w)
        throw std::invalid_argument("Nonexisting matrix element indexed.");

    if(!transposed)
        return denseMatrix[(idx.first - 1) * w + (idx.second - 1)];

    return denseMatrix[(idx.second - 1) * h + (idx.first - 1)];

}

template<typename T>
const T& Matrix<T>::operator [](std::pair<std::size_t, std::size_t> idx) const {
    if(idx.first < 1 || idx.second < 1 || idx.first > h || idx.second > w)
       throw std::invalid_argument("Nonexisting matrix element indexed.");

    if(!transposed)
        return denseMatrix[(idx.first - 1) * w + (idx.second - 1)];

    return denseMatrix[(idx.second - 1) * h + (idx.first - 1)];
}

template<typename T>
Matrix<T>& Matrix<T>::operator +=(const Matrix<T> &m) {
    if(!hasSameDimensionAs(m)) {
        Matrix<T>::throwBadMatrixDimensions();
    }

    for(std::size_t i = 1; i <= m.height(); ++i) {
        for(std::size_t j = 1; j <= m.width(); ++j) {
            this->operator[]({i, j}) += m[{i, j}];
        }
    }
    return *this;
}

template<typename T>
Matrix<T>& Matrix<T>::operator -=(const Matrix<T> &m) {
    if(!hasSameDimensionAs(m)) {
        Matrix<T>::throwBadMatrixDimensions();
    }

    for(std::size_t i = 1; i <= m.height(); ++i) {
        for(std::size_t j = 1; j <= m.width(); ++j) {
            this->operator[]({i, j}) -= m[{i, j}];
        }
    }
    return *this;
}

template<typename T>
Matrix<T> Matrix<T>::eye(std::size_t m, std::size_t n) {
    Matrix out(m, n);
    for(std::size_t i = 1; i <= out.height(); ++i) {
        for(std::size_t j = 1; j <= out.width(); ++j) {
            out[{i, j}] = 0;

            if(i == j) {
                out[{i, j}] = 1;
            }
        }
    }
    return out;
}

template<typename T>
Matrix<T> Matrix<T>::allK(std::size_t m, std::size_t n, std::size_t k) {
    Matrix out(m, n);
    for(std::size_t i = 1; i <= out.height(); ++i) {
        for(std::size_t j = 1; j <= out.width(); ++j) {
            out[{i,j}] = k;
        }
    }
    return out;
}

template<typename T>
bool Matrix<T>::hasSameDimensionAs(const Matrix<T>& m) const {
    return (width() == m.width()) && (height() == m.height());
}

template<typename T>
bool Matrix<T>::canBeMultipliedWith(const Matrix<T> &m) const {
    return width() == m.height();
}

template<typename T>
void Matrix<T>::changeDimensions(std::size_t m, std::size_t n) {
    h = m;
    w = n;
    denseMatrix.resize(m*n);
}

template<typename T>
void Matrix<T>::throwBadMatrixDimensions() {
    throw std::invalid_argument("Bad matrix dimension.");
}

template<typename T>
void Matrix<T>::throwNotInvertibleMatrix() {
    throw std::invalid_argument("Matrix is non-invertible.");
}

template<typename T>
Matrix<T> Matrix<T>::operator *=(const Matrix<T>& m) {
    Matrix<T> out = *this * m;
    changeDimensions(out.height(), out.width());
    for(std::size_t i = 1; i <= height(); ++i) {
        for(std::size_t j = 1; j <= width(); ++j)
            operator[]({i, j}) = out[{i, j}];

    }
    return *this;
}

template<typename T>
Matrix<T>& Matrix<T>::operator *=(const T& scalar) {
    for(std::size_t i = 1; i <= h; ++i) {
        for(std::size_t j = 1; j <= w; ++j)
            operator[]({i, j}) *= scalar;

    }
    return *this;
}

template<typename T>
Matrix<T>& Matrix<T>::operator /=(const T& scalar) {
    for(std::size_t i = 1; i <= h; ++i) {
        for(std::size_t j = 1; j <= w; ++j) {
            operator[]({i, j}) /= scalar;
        }
    }
    return *this;
}

template<typename T>
void Matrix<T>::transpose() {
    transposed = !transposed;
    std::size_t tmp = w;
    w = h;
    h = tmp;
}

template <typename T>
std::ostream& operator <<(std::ostream& os, const Matrix<T>& m) {
    for(std::size_t i = 1; i <= m.height(); ++i) {
        for(std::size_t j = 1; j <= m.width(); ++j) {
            os << m[{i, j}] << ' ';
        }
        os << '\n';
    }
    return os;
}

template <typename T>
Matrix<T> operator +(Matrix<T> lhs, const Matrix<T>& rhs) {
    lhs += rhs;
    return lhs;
}

template <typename T>
Matrix<T> operator -(Matrix<T> lhs, const Matrix<T>& rhs) {
    lhs -= rhs;
    return lhs;
}

template <typename T>
Matrix<T> operator *(const Matrix<T>& lhs, const Matrix<T>& rhs) {
    if(!lhs.canBeMultipliedWith(rhs))
        Matrix<T>::throwBadMatrixDimensions();

    Matrix<T> out(lhs.height(), rhs.width());

    for(std::size_t i = 1; i <= lhs.height(); ++i) {
        for(std::size_t j = 1; j <= rhs.width(); ++j) {
            T dotProduct = 0;
            for(std::size_t k = 1; k <= rhs.height(); ++k)
                dotProduct += lhs[{i,k}] * rhs[{k, j}];

            out[{i,j}] = dotProduct;
        }
    }
    return out;
}

template <typename T>
Matrix<T> operator *(const T& scalar, Matrix<T> m) {
    m *= scalar;
    return m;
}

template <typename T>
Matrix<T> operator *(Matrix<T> m, const T& scalar) {
    m *= scalar;
    return m;
}

template <typename T>
Matrix<T> operator /(Matrix<T> m, const T& scalar) {
    m /= scalar;
    return m;
}

template <typename T>
bool operator ==(const Matrix<T>& lhs, const Matrix<T>& rhs) {
    if(lhs.width() != rhs.width() || lhs.height() != rhs.height())
        Matrix<T>::throwBadMatrixDimensions();

    for(std::size_t i = 1; i <= lhs.height(); ++i) {
        for(std::size_t j = 1; j <= lhs.width(); ++j){
            if(lhs[{i, j}] != rhs[{i, j}])
                return false;

        }
    }
    return true;
}

template <typename T>
bool operator !=(const Matrix<T>& lhs, const Matrix<T>& rhs) {
    return !(lhs == rhs);
}

template<typename T>
std::vector<T> Matrix<T>::ref() {
    std::vector<T> accu;
    size_t l = 1;
    size_t k = 1;
    while(l <= h && k <= w) {
        size_t iMax = l;
        for(size_t i = l+1; i <= h; ++i){
            if(std::abs(operator[]({i, k})) > std::abs(operator[]({iMax, k}))) {
                iMax = i;
            }
        }
        if(operator[]({iMax, k}) == 0) {
            ++k;
        }
        else {
            swapRows(l, iMax);
            accu.emplace_back(-1);
            for(size_t i = l + 1; i <= h; ++i) {
                T f = operator[]({i, k}) / operator[]({l, k});
                operator[]({i, k}) = 0;
                for(size_t j = k + 1; j <= w; ++j) {
                    operator[]({i, j}) = operator[]({i, j}) - operator[]({k, j}) * f;
                }
            }
            ++l;
            ++k;
        }
    }
    return accu;
}

template<typename T>
void Matrix<T>::rowReduce() {
    std::size_t lead = 1;
    for(std::size_t r = 1; r <= h; ++r) {
        if(w < lead) {
            break;
        }
        std::size_t i = r;
        while(operator[]({i, lead}) == 0) {
            ++i;
            if(h == i) {
                i = r;
                ++lead;
                if(w == lead) {
                    --lead;
                    break;
                }

            }
        }
        if(i > h) return;
        swapRows(i, r);
        if(operator[]({r, lead}) != 0) {
            setRowTo(r, row(r) / operator[]({r, lead}));
        }
        for(std::size_t j = 1; j <= h; ++j) {
            if(j != r) {
                Matrix<T> k = row(j) - (operator[]({j, lead}) * row(r));
                setRowTo(j, row(j) - (operator[]({j, lead}) * row(r)));
            }
        }
        ++lead;
    }
}

template<typename T>
Matrix<T> Matrix<T>::inverse() const {
    if(w != h)
        throwNotInvertibleMatrix();

    Matrix<T> augmentedM(w, 2*w);
    augmentedM.setSubmatrix(1, 1, *this);
    augmentedM.setSubmatrix(1, w+1, eye(w, w));
    augmentedM.rowReduce();
    return augmentedM.submatrix(1, w+1, w, w);
}

template<typename T>
Matrix<T> Matrix<T>::row(std::size_t m) const {
    if(m > h) throw std::invalid_argument("Nonexisting row requested.");

    Matrix<T> out(1, w);
    for(std::size_t i = 1; i <= w; ++i) {
        out[{1, i}] = operator[]({m, i});
    }
    return out;
}

template<typename T>
Matrix<T> Matrix<T>::column(std::size_t n) const {
    if(n > w) throw std::invalid_argument("Nonexisting column requested.");

    Matrix<T> out(h, 1);
    for(std::size_t i = 1; i <= h; ++i)
        out[{i, 1}] = operator[]({i, n});

    return out;
}

template<typename T>
void Matrix<T>::setColumnTo(std::size_t n, const Matrix<T>& m) {
    if(m.width() != 1 || m.height() != height())
        throwBadMatrixDimensions();

    for(size_t i = 1; i <= h; ++i)
        operator[]({i, n}) = m[{i, 1}];
}

template<typename T>
void Matrix<T>::setRowTo(std::size_t m, const Matrix<T>& m1) {
    if(m1.height() != 1 || m1.width() != width())
        throwBadMatrixDimensions();

    for(std::size_t i = 1; i <= w; ++i)
        operator[]({m, i}) = m1[{1, i}];

}

template<typename T>
Matrix<T> Matrix<T>::submatrix(std::size_t i, std::size_t j, std::size_t m, std::size_t n) {
    if(i + m - 1 > h || j + n - 1 > w)
        throwBadMatrixDimensions();
    if(m < 1 || n < 1)
        throw std::invalid_argument("Bad submatrix size.");

    Matrix<T> out(m, n);
    for(std::size_t k = i; k < i + m; ++k) {
        for(std::size_t l = j; l < j + n; ++l)
            out[{k - i + 1, l - j + 1}] = operator[]({k, l});

    }
    return out;
}

template<typename T>
void Matrix<T>::setSubmatrix(std::size_t i, std::size_t j, const Matrix<T>& m) {
    if(i + m.height() - 1 > height() || j + m.width() - 1 > width())
        throwBadMatrixDimensions();
    if(i < 1 || j < 1)
        throw std::invalid_argument("Bad matrix indicies.");

    for(std::size_t k = i; k < i + m.height(); ++k) {
        for(std::size_t l = j; l < j + m.width(); ++l)
            operator[]({k, l}) = m[{k - i + 1, l - j + 1}];


    }
}

template<typename T>
void Matrix<T>::swapColumns(std::size_t i, std::size_t j) {
    Matrix<T> tmp = column(i);
    setColumnTo(i, column(j));
    setColumnTo(j, tmp);
}

template<typename T>
void Matrix<T>::swapRows(std::size_t i, std::size_t j) {
    Matrix<T> tmp = row(i);
    setRowTo(i, row(j));
    setRowTo(j, tmp);
}

template<typename T>
Matrix<T> Matrix<T>::lsm(const Matrix<T>& b) {
    Matrix<T> ATransposed = *this;
    ATransposed.transpose();
    return (ATransposed * (*this)).inverse() * ATransposed * b;
}

template<typename T>
bool Matrix<T>::isEye() const {
    for(std::size_t i = 1; i <= h; ++i) {
        for(std::size_t j = 1; j <= w; ++j) {
            if(i == j) {
                if(operator[]({i, j}) != 1)
                    return false;
            }
            else {
                if(operator[]({i, j}) != 0)
                    return false;
            }
        }
    }
    return true;
}

template<typename T>
bool Matrix<T>::isDiagonal() const {
    for(std::size_t i = 1; i <= h; ++i) {
        for(std::size_t j = 1; j <= w; ++j) {
            if(i == j) continue;

            if(operator[]({i, j}) != 0)
                return false;
        }
    }
    return true;
}

template<typename T>
bool Matrix<T>::isUpperTriangular() const {
    for(std::size_t i = 2; i <= h; ++i) {
        for(std::size_t j = 1; j < i; ++j) {
            if(operator[]({i, j}) != 0)
                return false;
        }
    }
    return true;
}

template<typename T>
bool Matrix<T>::isLowerTriangular() const {
    for(std::size_t j = 2; j <= w; ++j) {
        for(std::size_t i = 1; i < j; ++i) {
            if(operator[]({i, j}) != 0)
                return false;
        }
    }
    return true;
}

template<typename T>
T Matrix<T>::det() const {
    if(!isSquare())
        throwBadMatrixDimensions();

    T res = 1;
    Matrix<T> tmp = *this;
    std::vector<T> accu = tmp.ref();
    res = std::accumulate(accu.cbegin(), accu.cend(), res, std::multiplies<T>());
    for(std::size_t i = 1; i <= tmp.width(); ++i)
        res *= tmp[{i, i}];

    return res;
}

template<typename T>
bool Matrix<T>::isSquare() const {
    return w == h;
}

#endif //LSM_MATRIX_H
