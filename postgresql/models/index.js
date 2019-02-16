const fs = require("fs")
const path = require("path")
const Sequelize = require("sequelize")
require("dotenv").load({ path: ".env" })
const { JWT_SECRET } = process.env

const basename = path.basename(__filename)

const sequelize = new Sequelize(process.env.CONNECTION_STRING, {
  ssl: true,
  dialect: "postgres",
  dialectOptions: {
    ssl: true,
  },
  logging: false,
  pool: {
    max: process.env.NODE_ENV === "production" ? 50 : 5,
    min: process.env.NODE_ENV === "production" ? 20 : 1,
    idle: 200000,
    acquire: 400000,
    evict: 200000,
  },
})

const db = {}

const uppercaseFirstLetter = name => name[0].toUpperCase() + name.slice(1)

// load all models into db
fs
  .readdirSync(__dirname)
  .filter(
    file =>
      file.indexOf(".") !== 0 && file !== basename && file.slice(-3) === ".js"
  )
  .forEach(file => {
    const model = sequelize.import(path.join(__dirname, file))
    db[uppercaseFirstLetter(model.name)] = model
  })

Object.keys(db).forEach(modelName => {
  if (db[modelName].associate) {
    db[modelName].associate(db)
  }
})

db.sequelize = sequelize
db.Sequelize = Sequelize

module.exports = db
