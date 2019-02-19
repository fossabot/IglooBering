<p align="center">
  <img src="https://github.com/IglooCloud/IglooBering/blob/master/IglooLogo.png" alt="Igloo logo" width="128"/>
</p>

<h1 align="center">Igloo Bering</h1>

Bering is the back-end service of Igloo.

This service is still in development, some features may be missing. If your Igloo-based project needs a missing feature in order to work, please open an issue or help us by contributing.

## Getting started

### Installing

Start by cloning the repository and downloading the required libraries:

```bash
git clone https://github.com/hellowitlab/iglooBering
yarn
```

or

```bash
git clone https://github.com/hellowitlab/iglooBering
npm install
```

### Set Up

Create a new AWS ElasticBeanstalk application and follow the guided process, no need to upload the code right away, use the default demo application.

Once created go to configuration and click modify in the software tab, set the node command to `npm start`. In that safe configuration page you can set the environment variables (see below).

#### Environment variables

You will need to create a file `.env` or inject the variables in the environment yourself, the `.env` file should bee like this:

```
JWT_SECRET=
PUBLIC_VAPID_KEY=
PRIVATE_VAPID_KEY=
CONNECTION_STRING=
BUCKET_NAME=
AWS_ACCESS_KEY_ID=
AWS_SECRET_ACCESS_KEY=
STRIPE_SECRET_KEY=
```

The JWT secret can be whatever password you like, we suggest you [generate a random one](https://www.lastpass.com/password-generator). You can generate the VAPID key pair with [this web app](https://web-push-codelab.glitch.me/). The database URL is a PostgreSQL connection URL with this schema `postgresql://user:password@domain:port/dbname`. The Bucket name is the name of your S3 bucket.

##### Obtaining credentials

- Create credentials: https://docs.aws.amazon.com/sdk-for-javascript/v2/developer-guide/getting-your-credentials.html - create the new account, with access type _programmatic access_ - attach to the account the policy AmazonS3FullAccess and the policy AmazonSESFullAccess
- Save credentials locally: https://docs.aws.amazon.com/sdk-for-javascript/v2/developer-guide/loading-node-credentials-shared.html
- copy the credentials in the `.env` file

TODO: SES setup and S3 setup
TODO: Stripe setup

#### Database

We use Amazon RDS for PostgreSQL, but you can use any service or even host your own database.

##### Amazon RDS setup

If you host your server on heroku be sure to create the database in the US N. Virginia region, otherwise heroku won't be able to connect to it.

- open the RDS page in the AWS console
- click create database and follow the guided procedure - choose PostgreSQL as engine - if you host the server outside of AWS se the public accessibility to Yes
- at the end of the guided project go to the db instance
- wait for the database to be available (you see the status in the top summary)
- add in the env variable the CONNECTION_STRING formatted like this: `postgres://username:password@endpoint:port/database` (you find the endpoint in the connectivity tab)

##### Setup the database structure

To create the database structure needed just run the `db:create` sequelize command:

```bash
yarn sequelize db:create
```

or

```bash
npm run sequelize db:igloo
```

### Boot up

To start the development server:

```bash
yarn dev
```

or

```bash
npm run dev
```

To start the production server (the code gets transpiled and then run, instead of using the Babel interpreter):

```bash
yarn start
```

or

```bash
npm start
```

## Documentation

### General structure

Data on Igloo is organized hierarchically: the fundamental unit is the **Device** (e.g. thermometer, people counter, air pollution sensor, ...), every device has multiple **Value**s (e.g. temperature, humidity, CO2 concentration, ...). Multiple devices can be organized in **Environment**s (e.g. marine area, beehives, ...).

## Contributing

We are open to any contribution, just send us a pull request.

### Testing

You can run the automated tests using the `test` script, tests use a mocked database so they can be run offline.

```bash
yarn test
```

or

```bash
npm run test
```

## Questions

If you need something that Igloo does not support at the moment, please open an issue and we will discuss whether it fits the project.

## Acknowledgments

TBD

## License

See the [license](https://github.com/IglooCloud/IglooBering/blob/master/LICENSE.md) file for license rights and limitations.
