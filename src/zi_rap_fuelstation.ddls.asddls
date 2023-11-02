@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View: Fuel station'
define root view entity ZI_RAP_FuelStation
  as select from ztstation
  composition [1..*] of ZI_RAP_FuelPrice as _Prices
  association [1] to ZI_RAP_StationBrand as _Brand       on $projection.BrandUuid = _Brand.BrandUuid
  association [1] to ZI_RAP_StationType  as _StationType on $projection.StationUuid = _StationType.StationType
{
  key station_uuid          as StationUuid,
      station_id            as StationId,
      station_name          as StationName,
      brand_uuid            as BrandUuid,
      station_type          as StationType,
      municipality          as Municipality,
      district              as District,
      address               as Address,
      city                  as City,
      post_code             as PostCode,
      geolon                as Geolon,
      geolat                as Geolat,
      local_created_by      as LocalCreatedBy,
      local_created_at      as LocalCreatedAt,
      local_last_changed_by as LocalLastChangedBy,
      local_last_changed_at as LocalLastChangedAt,
      last_changed_at       as LastChangedAt,

      _Prices,
      _Brand,
      _StationType
}
