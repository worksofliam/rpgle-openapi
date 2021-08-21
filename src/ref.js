
//Used to resolve local $ref

exports.resolve = (totalBase, base) => {
    if (!base) base = totalBase;

    for (const key in base) {
        if (typeof base[key] === `object`) {
            if (base[key][`$ref`]) {
                base[key] = exports.parse(totalBase, base[key]);
            }

            exports.resolve(totalBase, base[key]);
        }
    }
}

exports.parse = (base, currentObject) => {

    if (currentObject['$ref']) {
        let current = base;

        const pieces = currentObject[`$ref`].split(`/`);

        for (const piece of pieces) {
            if (piece === `#`) {
                // ignore
            } else {
                current = current[piece];
            }
        }

        return current;
    } else {
        return currentObject
    }
}