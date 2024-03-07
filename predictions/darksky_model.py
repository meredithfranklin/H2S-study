

class DarkskyModel:

    def __init__(self, station_id, lon, lat, timestamp, weather_data):

        self.location_name = station_id
        self.timestamp = timestamp
        self.timezone = weather_data['timezone'] if weather_data.get('timezone') else 'NULL'
        self.summary = weather_data['summary'] if weather_data.get('summary') else 'NULL'
        self.icon = weather_data['icon'] if weather_data.get('icon') else 'NULL'
        self.precip_intensity = weather_data['precipIntensity'] if weather_data.get('precipIntensity') else 'NULL'
        self.precip_probability = weather_data['precipProbability'] if weather_data.get('precipProbability') else 'NULL'
        self.temperature = weather_data['temperature'] if weather_data.get('temperature') else 'NULL'
        self.apparent_temperature = weather_data['apparentTemperature'] if weather_data.get('apparentTemperature') else 'NULL'
        self.dew_point = weather_data['dewPoint'] if weather_data.get('dewPoint') else 'NULL'
        self.humidity = weather_data['humidity'] if weather_data.get('humidity') else 'NULL'
        self.pressure = weather_data['pressure'] if weather_data.get('pressure') else 'NULL'
        self.wind_speed = weather_data['windSpeed'] if weather_data.get('windSpeed') else 'NULL'
        self.wind_bearing = weather_data['windBearing'] if weather_data.get('windBearing') else 'NULL'
        self.cloud_cover = weather_data['cloudCover'] if weather_data.get('cloudCover') else 'NULL'
        self.uv_index = weather_data['uvIndex'] if weather_data.get('uvIndex') else 'NULL'
        self.visibility = weather_data['visibility'] if weather_data.get('visibility') else 'NULL'
        self.ozone = weather_data['ozone'] if weather_data.get('ozone') else 'NULL'
        self.lon = lon
        self.lat = lat
        self.location = self._get_point_geom(self.lon, self.lat)

    @staticmethod
    def _get_point_geom(x, y):
        """x: lon, y: lat"""
        point = 'ST_GeomFromText(\'POINT({lon} {lat})\', 4326)'.format(lon=x, lat=y)
        return point

    def members_to_str_list(self):
        return [str(self.location_name),
                '\'{}\''.format(str(self.timestamp)),
                '\'{}\''.format(self.summary), '\'{}\''.format(self.icon),
                str(self.precip_intensity), str(self.precip_probability), str(self.temperature),
                str(self.apparent_temperature), str(self.dew_point), str(self.humidity), str(self.pressure),
                str(self.wind_speed), str(self.wind_bearing), str(self.cloud_cover), str(self.uv_index),
                str(self.visibility), str(self.ozone), self.location]

