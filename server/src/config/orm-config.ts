import * as dotenv from "dotenv";
import * as path from "path";
import { DataSourceOptions } from "typeorm";

dotenv.config({
  path: path.resolve(process.cwd(), `.env`),
});

const migrationsPath = path.join(__dirname, "../migrations/*{.ts,.js}");
const entitiesPath = path.join(__dirname, "../**/*.entity{.ts,.js}");

const ormConfig: DataSourceOptions = {
  type: "postgres",
  host: process.env.DB_HOST,
  port: parseInt(process.env.DB_PORT ?? ""),
  username: process.env.DB_USER,
  password: process.env.DB_PASSWORD,
  database: process.env.DB_NAME,
  entities: [entitiesPath],
  synchronize: false,
  migrations: [migrationsPath],
};

export default ormConfig;
