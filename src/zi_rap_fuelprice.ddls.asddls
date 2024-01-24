@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View: Fuel price'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define view entity ZI_RAP_FuelPrice
  as select from ztfuel_price
  association to parent ZI_RAP_FuelStation as _Station on $projection.StationUuid = _Station.StationUuid
  association [0..1] to ZI_RAP_FuelType as _FuelType on $projection.FuelType = _FuelType.FuelTypeId
{
  key station_uuid as StationUuid,
  key price_uuid   as PriceUuid,
      price_id     as PriceID,
      fuel_type    as FuelType,
      update_date  as UpdateDate,
      price        as Price,

      _Station,
      _FuelType
}
