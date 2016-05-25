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


#if !defined(FIELD_HPP)
#define FIELD_HPP

#include <cstdio>
#include <iosfwd>
#include <string>
#include <vector>
#include "fit.h"
#include "fit_profile.h"

namespace fit
{

class Field
{
public:
    Field(void);
    Field(const Field &field);
    Field(const Profile::MESG_INDEX mesgIndex, const FIT_UINT16 fieldIndex);
    Field(const FIT_UINT16 mesgNum, const FIT_UINT8 fieldNum);
    Field(const std::string& mesgName, const std::string& fieldName);
    FIT_BOOL IsValid(void) const;
    FIT_BOOL GetIsAccumulated() const;
    FIT_UINT16 GetIndex(void) const;
    std::string GetName(const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD) const;
    FIT_UINT8 GetNum(void) const;
    FIT_UINT8 GetType(const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD) const;
    FIT_BOOL IsSignedInteger(const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD) const;
    std::string GetUnits(const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD) const;
    FIT_FLOAT64 GetScale(const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD) const;
    FIT_FLOAT64 GetOffset(const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD) const;
    FIT_UINT16 GetNumComponents(void) const;
    FIT_UINT16 GetNumSubFields(void) const;
    const Profile::FIELD_COMPONENT* GetComponent(const FIT_UINT16 component) const;
    const Profile::SUBFIELD* GetSubField(const FIT_UINT16 subFieldIndex) const;
    FIT_UINT8 GetSize(void) const;
    FIT_UINT8 GetNumValues(void) const;
    FIT_UINT32 GetBitsValue(const FIT_UINT16 offset, const FIT_UINT8 bits) const;
    FIT_SINT32 GetBitsSignedValue(const FIT_UINT16 offset, const FIT_UINT8 bits) const;
    FIT_BYTE GetValuesBYTE(const FIT_UINT8 index) const;
    FIT_SINT8 GetValuesSINT8(const FIT_UINT8 index) const;
    FIT_UINT8 GetValuesUINT8(const FIT_UINT8 index) const;
    FIT_FLOAT32 GetFLOAT32Value(const FIT_UINT8 fieldArrayIndex = 0, const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD) const;
    FIT_FLOAT64 GetFLOAT64Value(const FIT_UINT8 fieldArrayIndex = 0, const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD) const;
    FIT_WSTRING GetSTRINGValue(const FIT_UINT8 fieldArrayIndex = 0, const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD) const;
    FIT_FLOAT64 GetRawValue() const;
    FIT_FLOAT64 GetRawValue(const FIT_UINT8 fieldArrayIndex) const;
    FIT_ENUM GetENUMValue(const FIT_UINT8 fieldArrayIndex = 0) const;
    FIT_BYTE GetBYTEValue(const FIT_UINT8 fieldArrayIndex = 0) const;
    FIT_SINT8 GetSINT8Value(const FIT_UINT8 fieldArrayIndex = 0) const;
    FIT_UINT8 GetUINT8Value(const FIT_UINT8 fieldArrayIndex = 0) const;
    FIT_UINT8Z GetUINT8ZValue(const FIT_UINT8 fieldArrayIndex = 0) const;
    FIT_SINT16 GetSINT16Value(const FIT_UINT8 fieldArrayIndex = 0) const;
    FIT_UINT16 GetUINT16Value(const FIT_UINT8 fieldArrayIndex = 0) const;
    FIT_UINT16Z GetUINT16ZValue(const FIT_UINT8 fieldArrayIndex = 0) const;
    FIT_SINT32 GetSINT32Value(const FIT_UINT8 fieldArrayIndex = 0) const;
    FIT_UINT32 GetUINT32Value(const FIT_UINT8 fieldArrayIndex = 0) const;
    FIT_UINT32Z GetUINT32ZValue(const FIT_UINT8 fieldArrayIndex = 0) const;
    void AddRawValue(const FIT_FLOAT64 rawValue, const FIT_UINT8 fieldArrayIndex = 0);
    void SetENUMValue(const FIT_ENUM value, const FIT_UINT8 fieldArrayIndex = 0);
    void SetBYTEValue(const FIT_BYTE value, const FIT_UINT8 fieldArrayIndex = 0);
    void SetSINT8Value(const FIT_SINT8 value, const FIT_UINT8 fieldArrayIndex = 0);
    void SetUINT8Value(const FIT_UINT8 value, const FIT_UINT8 fieldArrayIndex = 0);
    void SetUINT8ZValue(const FIT_UINT8 value, const FIT_UINT8 fieldArrayIndex = 0);
    void SetSINT16Value(const FIT_SINT16 value, const FIT_UINT8 fieldArrayIndex = 0);
    void SetUINT16Value(const FIT_UINT16 value, const FIT_UINT8 fieldArrayIndex = 0);
    void SetUINT16ZValue(const FIT_UINT16Z value, const FIT_UINT8 fieldArrayIndex = 0);
    void SetSINT32Value(const FIT_SINT32 value, const FIT_UINT8 fieldArrayIndex = 0);
    void SetUINT32Value(const FIT_UINT32 value, const FIT_UINT8 fieldArrayIndex = 0);
    void SetUINT32ZValue(const FIT_UINT32Z value, const FIT_UINT8 fieldArrayIndex = 0);
    /**
     *
     * All deprecated functions will be removed in FIT 17.00
     *
    **/
    DEPRECATED FIT_ENUM GetENUMValue(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_BYTE GetBYTEValue(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_SINT8 GetSINT8Value(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_UINT8 GetUINT8Value(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_UINT8Z GetUINT8ZValue(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_SINT16 GetSINT16Value(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_UINT16 GetUINT16Value(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_UINT16Z GetUINT16ZValue(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_SINT32 GetSINT32Value(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_UINT32 GetUINT32Value(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_UINT32Z GetUINT32ZValue(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_FLOAT64 GetRawValue(const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex) const;
    DEPRECATED FIT_FLOAT64 GetRawValue(const FIT_UINT8 fieldArrayIndex, const std::string& subFieldName) const;
    void AddValue(const FIT_FLOAT64 value, const FIT_UINT8 fieldArrayIndex = 0, const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD);
    void SetFLOAT32Value(const FIT_FLOAT32 value, const FIT_UINT8 fieldArrayIndex = 0, const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD);
    void SetFLOAT64Value(const FIT_FLOAT64 value, const FIT_UINT8 fieldArrayIndex = 0, const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD);
    void SetSTRINGValue(const FIT_WSTRING& value, const FIT_UINT8 fieldArrayIndex = 0, const FIT_UINT16 subFieldIndex = FIT_SUBFIELD_INDEX_MAIN_FIELD);
    DEPRECATED void AddRawValue(const FIT_FLOAT64 rawValue, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    DEPRECATED void SetENUMValue(const FIT_ENUM value, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    DEPRECATED void SetBYTEValue(const FIT_BYTE value, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    DEPRECATED void SetSINT8Value(const FIT_SINT8 value, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    DEPRECATED void SetUINT8Value(const FIT_UINT8 value, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    DEPRECATED void SetUINT8ZValue(const FIT_UINT8 value, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    DEPRECATED void SetSINT16Value(const FIT_SINT16 value, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    DEPRECATED void SetUINT16Value(const FIT_UINT16 value, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    DEPRECATED void SetUINT16ZValue(const FIT_UINT16Z value, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    DEPRECATED void SetSINT32Value(const FIT_SINT32 value, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    DEPRECATED void SetUINT32Value(const FIT_UINT32 value, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    DEPRECATED void SetUINT32ZValue(const FIT_UINT32Z value, const FIT_UINT8 fieldArrayIndex, const FIT_UINT16 subFieldIndex);
    FIT_BOOL Read(const void *data, const FIT_UINT8 size);
    FIT_UINT8 Write(std::ostream &file) const;

protected:
    FIT_UINT16 GetSubField(const std::string& subFieldName) const;

private:
    FIT_FLOAT64 GetRawValueInternal(const FIT_UINT8 fieldArrayIndex = 0) const;
    static FIT_FLOAT64 Round(FIT_FLOAT64 value);

    const Profile::MESG* profile;
    FIT_UINT16 profileIndex;
    std::vector<std::vector<FIT_BYTE> > values;
};

} // namespace fit

#endif // defined(FIELD_HPP)
