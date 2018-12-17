const fs = require("fs")
const path = require("path")
const Sequelize = require("sequelize")
require("dotenv").load({ path: "../../.env" })
const { JWT_SECRET } = process.env

const basename = path.basename(__filename)

const sequelize = new Sequelize(process.env.DATABASE_URL, {
  ssl: true,
  dialect: "postgres",
  dialectOptions: {
    ssl: true,
  },
  logging: false,
  pool: {
    max: 20,
    min: 1,
    idle: 20000,
    acquire: 40000,
    evict: 20000,
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
