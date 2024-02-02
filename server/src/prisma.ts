
import { PrismaClient } from '@prisma/client'
import { config } from "./config";

export const prisma = new PrismaClient({
  datasourceUrl: config.DATABASE_URL
})
