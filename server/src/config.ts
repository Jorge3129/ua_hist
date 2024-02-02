import * as dotenv from "dotenv";
import path from "path";

dotenv.config({
  path: path.resolve(process.cwd(), `.dev.env`),
});

import { z } from "zod";

const configSchema = z.object({
  DATABASE_URL: z.string(),
})


export type Config = z.infer<typeof configSchema>;
export const config = configSchema.parse(process.env);
