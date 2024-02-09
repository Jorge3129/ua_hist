import { v4 as uuid } from "uuid";
import { TimedData, TimedDataPt, getForEventIndex } from "./timed-data";

export type MarkerInfo = {
  id: string;
  pos: [number, number];
  desc?: string;
  label?: string;
};

const markersWithDates: TimedData<MarkerInfo>[] = (<TimedDataPt<MarkerInfo>[]>[
  [
    {
      pos: [48.153889, 23.133056],
      desc: "Королевська стоянка",
      label: "Королевська стоянка",
    },
    0,
  ],
  [
    {
      pos: [46.981, 31.983],
      desc: "Місто людей кімерійських (Кімерополіс/Алібант)",
      label: "Кімерополіс",
    },
    11,
  ],
  [
    {
      pos: [46.7, 31.9],
      desc: "О́львія (дав.-гр. Ὀλβία — «Щаслива») (Сабія, далі Борисфеніда, ще далі Ольвія чи Ольвіополіс) — найважливіша грецька колонія в Нижньому Побужжі, у дельті Гіпаніса (Бугу) та Борисфена (Дніпра), заснована вихідцями з Мілета в 647—646 р. до н. е.",
      label: "Ὀλβία",
    },
    [14, 47],
  ],
  [
    {
      pos: [45.350833, 36.468611],
      desc: "Пантікапе́й (грец. Παντικάπαιον  можливо від іранського Panti-Kapa рибний шлях) — давньогрецький поліс, що існував на місці сучасного міста Керч; столиця Боспорського царства.",
      label: "Παντικάπαιον",
    },
    [14, 63],
  ],
  [
    {
      pos: [45.048889, 35.379167],
      desc: "",
      label: "Θεοδοσία",
    },
    [15, 63],
  ],
  [
    {
      pos: [45.188011, 33.373461],
      desc: "",
      label: "Κερκινίτις",
    },
    [15, 63],
  ],
  [
    {
      pos: [44.611667, 33.493333],
      desc: "",
      label: "Χερσόνησος",
    },
    [16, 63],
  ],
  [
    {
      pos: [46.201111, 30.350556],
      desc: "",
      label: "Τύρας",
    },
    [18, 47],
  ],
]).map(
  ([marker, timeInfo]): TimedData<MarkerInfo> => [
    { ...marker, id: uuid() },
    timeInfo,
  ]
);

export const getMarkers = getForEventIndex.bind(null, markersWithDates);
