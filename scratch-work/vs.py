import itertools


class VS:
    def update(self, i, o):
        pass

    def execute(self, i):
        pass


class TVS(VS):
    def __init__(self, vs, it, ot):
        self.vs = vs
        self.it = it
        self.ot = ot

    def update(self, i, o):
        self.vs.update(self.it(i), self.ot(o))

    def execute(self, i):
        return [self.ot(o) for o in self.vs.execute(self.it(i))]


class UVS(VS):
    def __init__(self, vss):
        self.vss = vss

    def update(self, i, o):
        for vs in self.vss:
            vs.update(i, o)

    def execute(self, i):
        return set([o for vs in self.vss for o in vs.execute(i)])


class LinearInt(VS):
    def __init__(self):
        self.hyp = [*range(-100, 100, 1)]

    def update(self, i, o):
        self.hyp = [o - i]

    def execute(self, i):
        return [i + h for h in self.hyp]


class Identity(VS):
    def update(self, i, o):
        return None

    def execute(self, i):
        return i


class Const(VS):
    def __init__(self):
        self.hyp = [*range(-100, 100, 1)]

    def update(self, i, o):
        self.hyp = [o]

    def execute(self, i):
        return self.hyp


class Document():
    def __init__(self, text, clipboard, location):
        self.text = text
        self.clipboard = clipboard
        self.location = location


def AbsCol():
    return TVS(Const(), lambda x: x, lambda x: x)


def RelCol():
    return TVS(LinearInt(), lambda x: x, lambda x: x)


def Col():
    return UVS([AbsCol(), RelCol()])


def AbsRow():
    return TVS(Const(), lambda x: x, lambda x: x)


def RelRow():
    return TVS(LinearInt(), lambda x: x, lambda x: x)


def Row():
    return UVS([AbsRow(), RelRow()])


class RowCol(VS):
    def __init__(self):
        self.row = Row()
        self.col = Col()

    def update(self, i, o):
        self.row.update(i.location[0], o[0])
        self.col.update(i.location[1], o[1])

    def execute(self, i):
        a = [self.row.execute(i.location[0]), self.col.execute(i.location[1])]

        return set(itertools.product(*a))
