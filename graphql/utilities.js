import unit from "smartcar-unit"
import length from "length.js"

const temperatureUnits = [
  "celsius",
  "centigrade",
  "fahrenheit",
  "kelvin",
  "kelvins",
  "c",
  "f",
  "k",
]
const weightUnits = [
  "ounce",
  "ounces",
  "pound",
  "pounds",
  "kilogram",
  "kilograms",
  "gram",
  "grams",
  "oz",
  "lb",
  "lbs",
  "kg",
  "g",
  "gm",
]
const volumeUnits = [
  "ml",
  "milliliter",
  "milliliters",
  "millilitre",
  "millilitres",
  "l",
  "liter",
  "liters",
  "litre",
  "litres",
  "usgal",
  "usgallon",
  "usgallons",
  "gal",
  "gallon",
  "gallons",
]
const pressureUnits = [
  "psi",
  "psis",
  "pa",
  "pascal",
  "pascals",
  "bar",
  "bars",
  "mmhg",
  "torr",
  "atmosphere",
  "atmospheres",
  "atmospheric",
  "barometric",
]
const lengthUnits = [
  "picometre",
  "nanometre",
  "micrometre",
  "millimetre",
  "metre",
  "kilometre",
  "nautical mile",
  "foot",
  "inch",
  "yard",
  "mile",
  "astronomical unit",
  "light year",
  "parsec",
  "pc",
  "pm",
  "nm",
  "um",
  "mm",
  "m",
  "km",
  "nmi",
  "ft",
  "in",
  "yd",
  "mi",
  "au",
  "ly",
  "pc",
]
const mapLengthUnits = {
  picometre: "pm",
  nanometre: "nm",
  micrometre: "um",
  millimetre: "mm",
  metre: "m",
  kilometre: "km",
  "nautical mile": "nmi",
  foot: "ft",
  inch: "in",
  yard: "yd",
  mile: "mi",
  "astronomical unit": "au",
  "light year": "ly",
  pc: "pc",
  pm: "pm",
  nm: "nm",
  um: "um",
  mm: "mm",
  m: "m",
  km: "km",
  nmi: "nmi",
  ft: "ft",
  in: "in",
  yd: "yd",
  mi: "mi",
  au: "au",
  ly: "ly",
  pc: "pc",
}

function unitType(_unit) {
  const unit = _unit.toLowerCase()
  if (temperatureUnits.indexOf(unit) !== -1) return "temperature"
  if (lengthUnits.indexOf(unit) !== -1) return "length"
  if (weightUnits.indexOf(unit) !== -1) return "weight"
  if (volumeUnits.indexOf(unit) !== -1) return "volume"
  if (pressureUnits.indexOf(unit) !== -1) return "pressure"
  else throw new Error("Unknown unit " + unit)
}

function convert(value, _from, _to) {
  const from = _from.toLowerCase()
  const to = _to.toLowerCase()

  if (unitType(from) !== unitType(to)) {
    throw new Error(`Cannot convert from ${from} to ${to}`)
  } else if (unitType(from) === "length") {
    return length(value, mapLengthUnits[from]).to(mapLengthUnits[to]).value
  } else {
    return unit(value, from).to(to)
  }
}

module.exports = { convert }
