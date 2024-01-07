import { MapContainer } from "react-leaflet/MapContainer";
import { Marker } from "react-leaflet/Marker";
import { Popup } from "react-leaflet/Popup";
import { TileLayer } from "react-leaflet/TileLayer";
import "./MapSection.scss";
import { Tooltip } from "react-leaflet/Tooltip";
import { GeoJSON } from "react-leaflet/GeoJSON";
import L from "leaflet";
import cityIcon from "../assets/city_icon.png";
import { bosporus1 } from "../data/bosporus";
import { MarkerInfo } from "../data/markers";
import { GeoInfo } from "../data/geojsons";

export function MapSection(props: { markers: MarkerInfo[]; geos: GeoInfo[] }) {
  const { markers, geos } = props;

  const myIcon = L.icon({
    iconUrl: cityIcon,
    iconSize: [15, 15],
    iconAnchor: [0, 0],
  });

  return (
    <div className="map_wrapper" style={{ height: "100vh" }}>
      <MapContainer
        style={{ height: "100vh" }}
        center={[50.4504, 30.5245]}
        zoom={6}
        scrollWheelZoom={true}
      >
        <TileLayer
          attribution='&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
          url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
        />
        {markers.map((marker) => (
          <Marker icon={myIcon} key={marker.id} position={marker.pos}>
            {marker.desc && <Popup>{marker.desc}</Popup>}
            {marker.label && (
              <Tooltip
                className="leaflet_tooltip"
                direction="right"
                offset={[5, 8]}
                opacity={1}
                permanent
              >
                {marker.label}
              </Tooltip>
            )}
          </Marker>
        ))}
        {geos.map((geo) => (
          <GeoJSON key={geo.id} data={geo.data} />
        ))}
      </MapContainer>
    </div>
  );
}
