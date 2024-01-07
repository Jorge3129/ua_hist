import { GeoJsonObject } from "geojson";
import { bosporus1, bosporus2, bosporus3, bosporus4 } from "./bosporus";
import { v4 as uuid } from "uuid";
import { TimedData, TimedDataPt, getForEventIndexCurried } from "./timed-data";

export type GeoInfo = {
  id: string;
  data: GeoJsonObject;
};

export type GeoWithDates = [geo: GeoInfo, timeInfo: number | [number, number?]];

export type GeoWithDatesPt = [
  geo: Omit<GeoInfo, "id">,
  timeInfo: number | [number, number?]
];

const geosWithDates: TimedData<GeoInfo>[] = (<TimedDataPt<GeoInfo>[]>[
  [
    {
      data: bosporus1,
    },
    [19, 23],
  ],
  [
    {
      data: bosporus3,
    },
    [23, 24],
  ],
  [
    {
      data: bosporus4,
    },
    [24],
  ],
]).map(([geo, timeInfo]): GeoWithDates => [{ ...geo, id: uuid() }, timeInfo]);

export const getGeos = getForEventIndexCurried(geosWithDates);
