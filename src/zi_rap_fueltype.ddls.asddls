@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View: Fuel type'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZI_RAP_FuelType as select from ztfuel_type
{
  key fuel_type_uuid as FuelTypeUuid,
  fuel_type_id as FuelTypeId,
  @Search.defaultSearchElement: true
  measure_unit as MeasureUnit,
  @Semantics.text: true
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.8
  fuel_type_name as FuelTypeName
}
