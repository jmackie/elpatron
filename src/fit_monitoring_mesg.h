////////////////////////////////////////////////////////////////////////////////
// The following FIT Protocol software provided may be used with FIT protocol
// devices only and remains the copyrighted property of Dynastream Innovations Inc.
// The software is being provided on an "as-is" basis and as an accommodation,
// and therefore all warranties, representations, or guarantees of any kind
// (whether express, implied or statutory) including, without limitation,
// warranties of merchantability, non-infringement, or fitness for a particular
// purpose, are specifically disclaimed.
//
// Copyright 2016 Dynastream Innovations Inc.
////////////////////////////////////////////////////////////////////////////////
// ****WARNING****  This file is auto-generated!  Do NOT edit this file.
// Profile Version = 16.73Release
// Tag = production-akw-16.73.00-0-gef88b3f
////////////////////////////////////////////////////////////////////////////////


#if !defined(FIT_MONITORING_MESG_HPP)
#define FIT_MONITORING_MESG_HPP

#include "fit_mesg.h"

namespace fit
{

class MonitoringMesg : public Mesg
{
public:
    MonitoringMesg(void) : Mesg(Profile::MESG_MONITORING)
    {
    }

    MonitoringMesg(const Mesg &mesg) : Mesg(mesg)
    {
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns timestamp field
    // Units: s
    // Comment: Must align to logging interval, for example, time must be 00:00:00 for daily log.
    ///////////////////////////////////////////////////////////////////////
    FIT_DATE_TIME GetTimestamp(void) const
    {
        return GetFieldUINT32Value(253, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set timestamp field
    // Units: s
    // Comment: Must align to logging interval, for example, time must be 00:00:00 for daily log.
    ///////////////////////////////////////////////////////////////////////
    void SetTimestamp(FIT_DATE_TIME timestamp)
    {
        SetFieldUINT32Value(253, timestamp, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns device_index field
    // Comment: Associates this data to device_info message.  Not required for file with single device (sensor).
    ///////////////////////////////////////////////////////////////////////
    FIT_DEVICE_INDEX GetDeviceIndex(void) const
    {
        return GetFieldUINT8Value(0, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set device_index field
    // Comment: Associates this data to device_info message.  Not required for file with single device (sensor).
    ///////////////////////////////////////////////////////////////////////
    void SetDeviceIndex(FIT_DEVICE_INDEX deviceIndex)
    {
        SetFieldUINT8Value(0, deviceIndex, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns calories field
    // Units: kcal
    // Comment: Accumulated total calories.  Maintained by MonitoringReader for each activity_type.  See SDK documentation
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT16 GetCalories(void) const
    {
        return GetFieldUINT16Value(1, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set calories field
    // Units: kcal
    // Comment: Accumulated total calories.  Maintained by MonitoringReader for each activity_type.  See SDK documentation
    ///////////////////////////////////////////////////////////////////////
    void SetCalories(FIT_UINT16 calories)
    {
        SetFieldUINT16Value(1, calories, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns distance field
    // Units: m
    // Comment: Accumulated distance.  Maintained by MonitoringReader for each activity_type.  See SDK documentation.
    ///////////////////////////////////////////////////////////////////////
    FIT_FLOAT32 GetDistance(void) const
    {
        return GetFieldFLOAT32Value(2, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set distance field
    // Units: m
    // Comment: Accumulated distance.  Maintained by MonitoringReader for each activity_type.  See SDK documentation.
    ///////////////////////////////////////////////////////////////////////
    void SetDistance(FIT_FLOAT32 distance)
    {
        SetFieldFLOAT32Value(2, distance, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns cycles field
    // Units: cycles
    // Comment: Accumulated cycles.  Maintained by MonitoringReader for each activity_type.  See SDK documentation.
    ///////////////////////////////////////////////////////////////////////
    FIT_FLOAT32 GetCycles(void) const
    {
        return GetFieldFLOAT32Value(3, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set cycles field
    // Units: cycles
    // Comment: Accumulated cycles.  Maintained by MonitoringReader for each activity_type.  See SDK documentation.
    ///////////////////////////////////////////////////////////////////////
    void SetCycles(FIT_FLOAT32 cycles)
    {
        SetFieldFLOAT32Value(3, cycles, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns steps field
    // Units: steps
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT32 GetSteps(void) const
    {
        return GetFieldUINT32Value(3, 0, (FIT_UINT16) Profile::MONITORING_MESG_CYCLES_FIELD_STEPS);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set steps field
    // Units: steps
    ///////////////////////////////////////////////////////////////////////
    void SetSteps(FIT_UINT32 steps)
    {
        SetFieldUINT32Value(3, steps, 0, (FIT_UINT16) Profile::MONITORING_MESG_CYCLES_FIELD_STEPS);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns strokes field
    // Units: strokes
    ///////////////////////////////////////////////////////////////////////
    FIT_FLOAT32 GetStrokes(void) const
    {
        return GetFieldFLOAT32Value(3, 0, (FIT_UINT16) Profile::MONITORING_MESG_CYCLES_FIELD_STROKES);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set strokes field
    // Units: strokes
    ///////////////////////////////////////////////////////////////////////
    void SetStrokes(FIT_FLOAT32 strokes)
    {
        SetFieldFLOAT32Value(3, strokes, 0, (FIT_UINT16) Profile::MONITORING_MESG_CYCLES_FIELD_STROKES);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns active_time field
    // Units: s
    ///////////////////////////////////////////////////////////////////////
    FIT_FLOAT32 GetActiveTime(void) const
    {
        return GetFieldFLOAT32Value(4, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set active_time field
    // Units: s
    ///////////////////////////////////////////////////////////////////////
    void SetActiveTime(FIT_FLOAT32 activeTime)
    {
        SetFieldFLOAT32Value(4, activeTime, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns activity_type field
    ///////////////////////////////////////////////////////////////////////
    FIT_ACTIVITY_TYPE GetActivityType(void) const
    {
        return GetFieldENUMValue(5, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set activity_type field
    ///////////////////////////////////////////////////////////////////////
    void SetActivityType(FIT_ACTIVITY_TYPE activityType)
    {
        SetFieldENUMValue(5, activityType, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns activity_subtype field
    ///////////////////////////////////////////////////////////////////////
    FIT_ACTIVITY_SUBTYPE GetActivitySubtype(void) const
    {
        return GetFieldENUMValue(6, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set activity_subtype field
    ///////////////////////////////////////////////////////////////////////
    void SetActivitySubtype(FIT_ACTIVITY_SUBTYPE activitySubtype)
    {
        SetFieldENUMValue(6, activitySubtype, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns activity_level field
    ///////////////////////////////////////////////////////////////////////
    FIT_ACTIVITY_LEVEL GetActivityLevel(void) const
    {
        return GetFieldENUMValue(7, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set activity_level field
    ///////////////////////////////////////////////////////////////////////
    void SetActivityLevel(FIT_ACTIVITY_LEVEL activityLevel)
    {
        SetFieldENUMValue(7, activityLevel, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns distance_16 field
    // Units: 100 * m
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT16 GetDistance16(void) const
    {
        return GetFieldUINT16Value(8, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set distance_16 field
    // Units: 100 * m
    ///////////////////////////////////////////////////////////////////////
    void SetDistance16(FIT_UINT16 distance16)
    {
        SetFieldUINT16Value(8, distance16, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns cycles_16 field
    // Units: 2 * cycles (steps)
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT16 GetCycles16(void) const
    {
        return GetFieldUINT16Value(9, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set cycles_16 field
    // Units: 2 * cycles (steps)
    ///////////////////////////////////////////////////////////////////////
    void SetCycles16(FIT_UINT16 cycles16)
    {
        SetFieldUINT16Value(9, cycles16, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns active_time_16 field
    // Units: s
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT16 GetActiveTime16(void) const
    {
        return GetFieldUINT16Value(10, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set active_time_16 field
    // Units: s
    ///////////////////////////////////////////////////////////////////////
    void SetActiveTime16(FIT_UINT16 activeTime16)
    {
        SetFieldUINT16Value(10, activeTime16, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns local_timestamp field
    // Comment: Must align to logging interval, for example, time must be 00:00:00 for daily log.
    ///////////////////////////////////////////////////////////////////////
    FIT_LOCAL_DATE_TIME GetLocalTimestamp(void) const
    {
        return GetFieldUINT32Value(11, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set local_timestamp field
    // Comment: Must align to logging interval, for example, time must be 00:00:00 for daily log.
    ///////////////////////////////////////////////////////////////////////
    void SetLocalTimestamp(FIT_LOCAL_DATE_TIME localTimestamp)
    {
        SetFieldUINT32Value(11, localTimestamp, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns temperature field
    // Units: C
    // Comment: Avg temperature during the logging interval ended at timestamp
    ///////////////////////////////////////////////////////////////////////
    FIT_FLOAT32 GetTemperature(void) const
    {
        return GetFieldFLOAT32Value(12, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set temperature field
    // Units: C
    // Comment: Avg temperature during the logging interval ended at timestamp
    ///////////////////////////////////////////////////////////////////////
    void SetTemperature(FIT_FLOAT32 temperature)
    {
        SetFieldFLOAT32Value(12, temperature, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns temperature_min field
    // Units: C
    // Comment: Min temperature during the logging interval ended at timestamp
    ///////////////////////////////////////////////////////////////////////
    FIT_FLOAT32 GetTemperatureMin(void) const
    {
        return GetFieldFLOAT32Value(14, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set temperature_min field
    // Units: C
    // Comment: Min temperature during the logging interval ended at timestamp
    ///////////////////////////////////////////////////////////////////////
    void SetTemperatureMin(FIT_FLOAT32 temperatureMin)
    {
        SetFieldFLOAT32Value(14, temperatureMin, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns temperature_max field
    // Units: C
    // Comment: Max temperature during the logging interval ended at timestamp
    ///////////////////////////////////////////////////////////////////////
    FIT_FLOAT32 GetTemperatureMax(void) const
    {
        return GetFieldFLOAT32Value(15, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set temperature_max field
    // Units: C
    // Comment: Max temperature during the logging interval ended at timestamp
    ///////////////////////////////////////////////////////////////////////
    void SetTemperatureMax(FIT_FLOAT32 temperatureMax)
    {
        SetFieldFLOAT32Value(15, temperatureMax, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns number of activity_time
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT8 GetNumActivityTime(void) const
    {
        return GetFieldNumValues(16, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns activity_time field
    // Units: minutes
    // Comment: Indexed using minute_activity_level enum
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT16 GetActivityTime(FIT_UINT8 index) const
    {
        return GetFieldUINT16Value(16, index, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set activity_time field
    // Units: minutes
    // Comment: Indexed using minute_activity_level enum
    ///////////////////////////////////////////////////////////////////////
    void SetActivityTime(FIT_UINT8 index, FIT_UINT16 activityTime)
    {
        SetFieldUINT16Value(16, activityTime, index, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns active_calories field
    // Units: kcal
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT16 GetActiveCalories(void) const
    {
        return GetFieldUINT16Value(19, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set active_calories field
    // Units: kcal
    ///////////////////////////////////////////////////////////////////////
    void SetActiveCalories(FIT_UINT16 activeCalories)
    {
        SetFieldUINT16Value(19, activeCalories, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns current_activity_type_intensity field
    // Comment: Indicates single type / intensity for duration since last monitoring message.
    ///////////////////////////////////////////////////////////////////////
    FIT_BYTE GetCurrentActivityTypeIntensity(void) const
    {
        return GetFieldBYTEValue(24, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set current_activity_type_intensity field
    // Comment: Indicates single type / intensity for duration since last monitoring message.
    ///////////////////////////////////////////////////////////////////////
    void SetCurrentActivityTypeIntensity(FIT_BYTE currentActivityTypeIntensity)
    {
        SetFieldBYTEValue(24, currentActivityTypeIntensity, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns timestamp_min_8 field
    // Units: min
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT8 GetTimestampMin8(void) const
    {
        return GetFieldUINT8Value(25, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set timestamp_min_8 field
    // Units: min
    ///////////////////////////////////////////////////////////////////////
    void SetTimestampMin8(FIT_UINT8 timestampMin8)
    {
        SetFieldUINT8Value(25, timestampMin8, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns timestamp_16 field
    // Units: s
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT16 GetTimestamp16(void) const
    {
        return GetFieldUINT16Value(26, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set timestamp_16 field
    // Units: s
    ///////////////////////////////////////////////////////////////////////
    void SetTimestamp16(FIT_UINT16 timestamp16)
    {
        SetFieldUINT16Value(26, timestamp16, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns heart_rate field
    // Units: bpm
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT8 GetHeartRate(void) const
    {
        return GetFieldUINT8Value(27, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set heart_rate field
    // Units: bpm
    ///////////////////////////////////////////////////////////////////////
    void SetHeartRate(FIT_UINT8 heartRate)
    {
        SetFieldUINT8Value(27, heartRate, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns intensity field
    ///////////////////////////////////////////////////////////////////////
    FIT_FLOAT32 GetIntensity(void) const
    {
        return GetFieldFLOAT32Value(28, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set intensity field
    ///////////////////////////////////////////////////////////////////////
    void SetIntensity(FIT_FLOAT32 intensity)
    {
        SetFieldFLOAT32Value(28, intensity, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns duration_min field
    // Units: min
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT16 GetDurationMin(void) const
    {
        return GetFieldUINT16Value(29, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set duration_min field
    // Units: min
    ///////////////////////////////////////////////////////////////////////
    void SetDurationMin(FIT_UINT16 durationMin)
    {
        SetFieldUINT16Value(29, durationMin, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns duration field
    // Units: s
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT32 GetDuration(void) const
    {
        return GetFieldUINT32Value(30, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set duration field
    // Units: s
    ///////////////////////////////////////////////////////////////////////
    void SetDuration(FIT_UINT32 duration)
    {
        SetFieldUINT32Value(30, duration, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns ascent field
    // Units: m
    ///////////////////////////////////////////////////////////////////////
    FIT_FLOAT32 GetAscent(void) const
    {
        return GetFieldFLOAT32Value(31, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set ascent field
    // Units: m
    ///////////////////////////////////////////////////////////////////////
    void SetAscent(FIT_FLOAT32 ascent)
    {
        SetFieldFLOAT32Value(31, ascent, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns descent field
    // Units: m
    ///////////////////////////////////////////////////////////////////////
    FIT_FLOAT32 GetDescent(void) const
    {
        return GetFieldFLOAT32Value(32, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set descent field
    // Units: m
    ///////////////////////////////////////////////////////////////////////
    void SetDescent(FIT_FLOAT32 descent)
    {
        SetFieldFLOAT32Value(32, descent, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns moderate_activity_minutes field
    // Units: minutes
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT16 GetModerateActivityMinutes(void) const
    {
        return GetFieldUINT16Value(33, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set moderate_activity_minutes field
    // Units: minutes
    ///////////////////////////////////////////////////////////////////////
    void SetModerateActivityMinutes(FIT_UINT16 moderateActivityMinutes)
    {
        SetFieldUINT16Value(33, moderateActivityMinutes, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Returns vigorous_activity_minutes field
    // Units: minutes
    ///////////////////////////////////////////////////////////////////////
    FIT_UINT16 GetVigorousActivityMinutes(void) const
    {
        return GetFieldUINT16Value(34, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

    ///////////////////////////////////////////////////////////////////////
    // Set vigorous_activity_minutes field
    // Units: minutes
    ///////////////////////////////////////////////////////////////////////
    void SetVigorousActivityMinutes(FIT_UINT16 vigorousActivityMinutes)
    {
        SetFieldUINT16Value(34, vigorousActivityMinutes, 0, FIT_SUBFIELD_INDEX_MAIN_FIELD);
    }

};

} // namespace fit

#endif // !defined(FIT_MONITORING_MESG_HPP)
